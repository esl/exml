#ifndef RAPIDXML_STATIC_POOL_SIZE
#define RAPIDXML_STATIC_POOL_SIZE (10 * 1024 * 1024)
#endif
#ifndef RAPIDXML_DYNAMIC_POOL_SIZE
#define RAPIDXML_DYNAMIC_POOL_SIZE (2 * 1024 * 1024)
#endif

#include "rapidxml.hpp"
#include "rapidxml_print.hpp"
#include <erl_nif.h>

#include <algorithm>
#include <chrono>
#include <cstring>
#include <iostream>
#include <string>
#include <thread>
#include <vector>

using ustring = std::basic_string<unsigned char>;

class xml_document {
public:
  struct ParseResult {
    bool eof = false;
    bool has_error = false;
    std::string error_message;
    const unsigned char *rest = nullptr;
  };

  template <int flags>
  ParseResult parse(unsigned char *text, xml_document &parent) {
    return with_error_handling(
        [&] { return impl.parse<flags>(text, parent.impl); });
  }

  template <int flags> ParseResult parse(unsigned char *text) {
    return with_error_handling([&] { return impl.parse<flags>(text); });
  }

  const void clear() { impl.clear(); }

  rapidxml::xml_document<unsigned char> impl;

private:
  template <typename F> ParseResult with_error_handling(F &&f) {
    ParseResult result;
    try {
      result.rest = std::forward<F>(f)();
    } catch (const rapidxml::eof_error &e) {
      result.eof = true;
      result.has_error = true;
      result.error_message = e.what();
    } catch (const rapidxml::parse_error &e) {
      result.has_error = true;
      result.error_message = e.what();
    }
    return result;
  }
};

namespace {
  ERL_NIF_TERM atom_ok;
  ERL_NIF_TERM atom_error;
  ERL_NIF_TERM atom_undefined;
  ERL_NIF_TERM atom_xmlel;
  ERL_NIF_TERM atom_xmlcdata;
  ERL_NIF_TERM atom_xmlstreamstart;
  ERL_NIF_TERM atom_xmlstreamend;
  ERL_NIF_TERM atom_pretty;
  ERL_NIF_TERM atom_true;
  constexpr const unsigned char EMPTY[1] = {0};

  xml_document &get_static_doc() {
    static thread_local xml_document doc;
    doc.clear();
    return doc;
  }

} // namespace

struct Parser {
  ustring stream_tag;
  std::uint64_t max_element_size = 0;
  bool infinite_stream = false;

  static thread_local std::vector<unsigned char> buffer;
  static thread_local std::vector<ERL_NIF_TERM> term_buffer;

  bool copy_buffer(ErlNifEnv *env, ERL_NIF_TERM buf) {
    buffer.clear();

    ErlNifBinary bin;
    if (enif_inspect_binary(env, buf, &bin)) {
      buffer.insert(buffer.end(), bin.data, bin.data + bin.size);
    } else if (enif_is_list(env, buf)) {
      for (ERL_NIF_TERM head; enif_get_list_cell(env, buf, &head, &buf);) {
        if (!enif_inspect_binary(env, head, &bin))
          return false;

        buffer.insert(buffer.end(), bin.data, bin.data + bin.size);
      }
    } else {
      return false;
    }

    buffer.push_back('\0');
    return true;
  }

  void reset() {
    stream_tag.clear();
    buffer.clear();
  }
};

thread_local std::vector<unsigned char> Parser::buffer;
thread_local std::vector<ERL_NIF_TERM> Parser::term_buffer;

struct ParseCtx {
  ErlNifEnv *env;
  Parser *parser;
};

namespace {
ErlNifResourceType *parser_type;

constexpr int default_parse_flags() {
  return rapidxml::parse_no_string_terminators | rapidxml::parse_validate_control_chars;
}

constexpr int parse_one() {
  return rapidxml::parse_parse_one | default_parse_flags();
}

constexpr int parse_open_only() {
  return rapidxml::parse_open_only | default_parse_flags();
}

ERL_NIF_TERM to_subbinary(ParseCtx &ctx, const unsigned char *text,
                          std::size_t len) {
  ERL_NIF_TERM binary;
  unsigned char *bin_data = enif_make_new_binary(ctx.env, len, &binary);
  std::copy(text, text + len, bin_data);
  return binary;
}

ERL_NIF_TERM make_attr_tuple(ParseCtx &ctx,
                             rapidxml::xml_attribute<unsigned char> *attr) {
  ERL_NIF_TERM name = to_subbinary(ctx, attr->name(), attr->name_size());
  ERL_NIF_TERM value = to_subbinary(ctx, attr->value(), attr->value_size());
  return enif_make_tuple2(ctx.env, name, value);
}

ERL_NIF_TERM get_xmlcdata(ParseCtx &ctx,
                          rapidxml::xml_node<unsigned char> *node) {
  return enif_make_tuple2(ctx.env, atom_xmlcdata,
                          to_subbinary(ctx, node->value(), node->value_size()));
}

ERL_NIF_TERM merge_data_nodes(ParseCtx &ctx,
                              rapidxml::xml_node<unsigned char> *node,
                              std::size_t total_size) {
  ERL_NIF_TERM bin;
  unsigned char *it = enif_make_new_binary(ctx.env, total_size, &bin);

  while (total_size > 0) {
    it = std::copy(node->value(), node->value() + node->value_size(), it);
    total_size -= node->value_size();
    node = node->next_sibling();
  }

  return enif_make_tuple2(ctx.env, atom_xmlcdata, bin);
}

void append_pending_data_nodes(ParseCtx &ctx,
                               std::vector<ERL_NIF_TERM> &children,
                               rapidxml::xml_node<unsigned char> *node,
                               const std::size_t pending) {
  if (pending == 0)
    return;

  if (pending == node->value_size())
    children.push_back(get_xmlcdata(ctx, node));
  else
    children.push_back(merge_data_nodes(ctx, node, pending));
}

ERL_NIF_TERM make_xmlel(ParseCtx &ctx, rapidxml::xml_node<unsigned char> *node);

ERL_NIF_TERM get_children_tuple(ParseCtx &ctx,
                                rapidxml::xml_node<unsigned char> *node) {
  std::vector<ERL_NIF_TERM> &children = Parser::term_buffer;
  std::size_t begin = children.size();

  rapidxml::xml_node<unsigned char> *first_data_node = nullptr;
  std::size_t pending_data_size = 0;

  for (rapidxml::xml_node<unsigned char> *child = node->first_node(); child;
       child = child->next_sibling()) {
    const bool is_data_node = child->type() == rapidxml::node_data ||
                              child->type() == rapidxml::node_cdata;

    if (is_data_node) {
      if (pending_data_size == 0)
        first_data_node = child;
      pending_data_size += child->value_size();
    } else {
      append_pending_data_nodes(ctx, children, first_data_node,
                                pending_data_size);
      pending_data_size = 0;
      if (child->type() == rapidxml::node_element)
        children.push_back(make_xmlel(ctx, child));
    }
  }

  append_pending_data_nodes(ctx, children, first_data_node, pending_data_size);

  std::size_t size = children.size() - begin;
  if (size == 0)
    return enif_make_list(ctx.env, 0);

  ERL_NIF_TERM arr =
      enif_make_list_from_array(ctx.env, children.data() + begin, size);
  children.erase(children.end() - size, children.end());
  return arr;
}

std::pair<const unsigned char *, std::size_t>
node_name(rapidxml::xml_node<unsigned char> *node) {
  const unsigned char *start = node->name();
  std::size_t len = node->name_size();
  if (node->prefix()) {
    start = node->prefix();
    len += node->prefix_size() + 1;
  }
  return {start, len};
}

ERL_NIF_TERM make_node_name_binary(ParseCtx &ctx,
                                   rapidxml::xml_node<unsigned char> *node) {
  const unsigned char *start;
  std::size_t len;
  std::tie(start, len) = node_name(node);
  return to_subbinary(ctx, start, len);
}

std::tuple<ERL_NIF_TERM, ERL_NIF_TERM>
get_open_tag(ParseCtx &ctx, rapidxml::xml_node<unsigned char> *node) {
  ERL_NIF_TERM name_term = make_node_name_binary(ctx, node);
  std::vector<ERL_NIF_TERM> &attrs = Parser::term_buffer;
  std::size_t begin = attrs.size();

  for (rapidxml::xml_attribute<unsigned char> *attr = node->first_attribute();
       attr; attr = attr->next_attribute())
    attrs.push_back(make_attr_tuple(ctx, attr));

  std::size_t size = attrs.size() - begin;
  ERL_NIF_TERM attrs_term =
      size == 0
          ? enif_make_list(ctx.env, 0)
          : enif_make_list_from_array(ctx.env, attrs.data() + begin, size);

  attrs.erase(attrs.end() - size, attrs.end());
  return std::make_tuple(name_term, attrs_term);
}

ERL_NIF_TERM make_stream_start_tuple(ParseCtx &ctx,
                                     rapidxml::xml_node<unsigned char> *node) {
  auto name_and_attrs = get_open_tag(ctx, node);
  return enif_make_tuple3(ctx.env, atom_xmlstreamstart, std::get<0>(name_and_attrs),
                          std::get<1>(name_and_attrs));
}

ERL_NIF_TERM make_stream_end_tuple(ParseCtx &ctx) {
  ERL_NIF_TERM name;
  unsigned char *data =
      enif_make_new_binary(ctx.env, ctx.parser->stream_tag.size(), &name);

  std::copy(ctx.parser->stream_tag.begin(), ctx.parser->stream_tag.end(), data);

  return enif_make_tuple2(ctx.env, atom_xmlstreamend, name);
}

ERL_NIF_TERM make_xmlel(ParseCtx &ctx,
                        rapidxml::xml_node<unsigned char> *node) {
  auto name_and_attrs = get_open_tag(ctx, node);
  ERL_NIF_TERM children_term = get_children_tuple(ctx, node);
  return enif_make_tuple4(ctx.env, atom_xmlel, std::get<0>(name_and_attrs),
                          std::get<1>(name_and_attrs), children_term);
}

bool build_children(ErlNifEnv *env, xml_document &doc, ERL_NIF_TERM children,
                    rapidxml::xml_node<unsigned char> &node);

bool build_cdata(ErlNifEnv *env, xml_document &doc, const ERL_NIF_TERM elem[],
                 rapidxml::xml_node<unsigned char> &node) {
  ErlNifBinary bin;
  if (!enif_inspect_iolist_as_binary(env, elem[1], &bin))
    return false;

  auto child = doc.impl.allocate_node(rapidxml::node_data);
  child->value(bin.size > 0 ? bin.data : EMPTY, bin.size);
  node.append_node(child);
  return true;
}

bool build_attrs(ErlNifEnv *env, xml_document &doc, ERL_NIF_TERM attrs,
                 rapidxml::xml_node<unsigned char> &node) {

  if (!enif_is_list(env, attrs))
    return false;

  for (ERL_NIF_TERM head; enif_get_list_cell(env, attrs, &head, &attrs);) {
    int arity;
    const ERL_NIF_TERM *tuple;
    if (!enif_get_tuple(env, head, &arity, &tuple) || arity != 2)
      return false;

    ErlNifBinary key, value;
    if (!enif_inspect_iolist_as_binary(env, tuple[0], &key))
      return false;

    if (!enif_inspect_iolist_as_binary(env, tuple[1], &value))
      return false;

    auto attr = doc.impl.allocate_attribute(key.size > 0 ? key.data : EMPTY,
                                            value.size > 0 ? value.data : EMPTY,
                                            key.size, value.size);
    node.append_attribute(attr);
  }

  return true;
}

bool build_el(ErlNifEnv *env, xml_document &doc, const ERL_NIF_TERM elem[],
              rapidxml::xml_node<unsigned char> &node) {
  ErlNifBinary name;
  if (!enif_inspect_iolist_as_binary(env, elem[1], &name))
    return false;

  auto child = doc.impl.allocate_node(rapidxml::node_element);
  child->name(name.size > 0 ? name.data : EMPTY, name.size);
  node.append_node(child);

  if (!build_attrs(env, doc, elem[2], *child))
    return false;
  if (!build_children(env, doc, elem[3], *child))
    return false;

  return true;
}

bool build_child(ErlNifEnv *env, xml_document &doc, ERL_NIF_TERM child,
                 rapidxml::xml_node<unsigned char> &node) {
  int arity;
  const ERL_NIF_TERM *tuple;
  if (!enif_get_tuple(env, child, &arity, &tuple))
    return false;

  if (arity == 2 && enif_compare(atom_xmlcdata, tuple[0]) == 0) {
    if (!build_cdata(env, doc, tuple, node))
      return false;
  } else if (arity == 4 && enif_compare(atom_xmlel, tuple[0]) == 0) {
    if (!build_el(env, doc, tuple, node))
      return false;
  } else {
    return false;
  }

  return true;
}

bool build_children(ErlNifEnv *env, xml_document &doc, ERL_NIF_TERM children,
                    rapidxml::xml_node<unsigned char> &node) {

  if (!enif_is_list(env, children))
    return false;

  for (ERL_NIF_TERM head;
       enif_get_list_cell(env, children, &head, &children);) {
    if (!build_child(env, doc, head, node))
      return false;
  }

  return true;
}

ERL_NIF_TERM node_to_binary(ErlNifEnv *env,
                            rapidxml::xml_node<unsigned char> &node,
                            int flags) {
  static thread_local std::vector<unsigned char> print_buffer;
  print_buffer.clear();

  rapidxml::print(std::back_inserter(print_buffer), node, flags);

  ERL_NIF_TERM ret_binary;
  unsigned char *data =
      enif_make_new_binary(env, print_buffer.size(), &ret_binary);
  std::copy(print_buffer.begin(), print_buffer.end(), data);
  return ret_binary;
}

std::size_t stream_closing_tag_size(Parser *parser) {
  return 3 + parser->stream_tag.size(); // name + </>
}

bool has_stream_closing_tag(Parser *parser, std::size_t offset) {
  if (Parser::buffer.size() < offset + stream_closing_tag_size(parser))
    return false;

  if (Parser::buffer[offset] != '<' || Parser::buffer[offset + 1] != '/')
    return false;

  if (!std::equal(parser->stream_tag.begin(), parser->stream_tag.end(),
                  Parser::buffer.begin() + offset + 2))
    return false;

  // skip whitespace between tag name and closing '>'
  offset = offset + 2 + parser->stream_tag.size();
  while (offset < Parser::buffer.size() - 1 &&
         std::isspace(Parser::buffer[offset]))
    ++offset;

  return Parser::buffer[offset] == '>';
}

} // namespace

extern "C" {
static void delete_parser(ErlNifEnv *env, void *parser) {
  static_cast<Parser *>(parser)->~Parser();
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  parser_type = enif_open_resource_type(
      env, "exml_nif", "parser", &delete_parser, ERL_NIF_RT_CREATE, nullptr);
  atom_ok = enif_make_atom(env, "ok");
  atom_error = enif_make_atom(env, "error");
  atom_undefined = enif_make_atom(env, "undefined");
  atom_xmlel = enif_make_atom(env, "xmlel");
  atom_xmlcdata = enif_make_atom(env, "xmlcdata");
  atom_xmlstreamstart = enif_make_atom(env, "xmlstreamstart");
  atom_xmlstreamend = enif_make_atom(env, "xmlstreamend");
  atom_pretty = enif_make_atom(env, "pretty");
  atom_true = enif_make_atom(env, "true");

  get_static_doc().impl.set_allocator(enif_alloc, enif_free);

  return 0;
}

static void unload(ErlNifEnv *env, void *priv_data) {
  return;
}

static ERL_NIF_TERM create(ErlNifEnv *env, int argc,
                           const ERL_NIF_TERM argv[]) {
  void *mem = enif_alloc_resource(parser_type, sizeof(Parser));
  Parser *parser = new (mem) Parser;

  ErlNifUInt64 max_element_size;
  if (!enif_get_uint64(env, argv[0], &max_element_size))
    return enif_make_badarg(env);
  parser->max_element_size = static_cast<std::uint64_t>(max_element_size);
  if (enif_compare(atom_true, argv[1]) == 0)
    parser->infinite_stream = true;

  ERL_NIF_TERM term = enif_make_resource(env, parser);
  enif_release_resource(parser);
  return enif_make_tuple2(env, atom_ok, term);
}

static ERL_NIF_TERM parse_next(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  Parser *parser;
  if (!enif_get_resource(env, argv[0], parser_type,
                         reinterpret_cast<void **>(&parser)))
    return enif_make_badarg(env);

  if (!parser->copy_buffer(env, argv[1]))
    return enif_make_badarg(env);

  // Skip initial whitespace even if we don't manage to parse anything.
  // Also needed for has_stream_closing_tag to recognize the tag.
  std::size_t offset = 0;
  while (offset < Parser::buffer.size() - 1 &&
         std::isspace(Parser::buffer[offset]))
    ++offset;

  ParseCtx ctx{env, parser};
  xml_document::ParseResult result;
  ERL_NIF_TERM element;
  const char *error_msg = nullptr;

  xml_document &doc = get_static_doc();
  Parser::term_buffer.clear();

  auto parseStreamOpen = [&] {
    result = doc.parse<parse_open_only()>(Parser::buffer.data() + offset);
    if (!result.has_error) {
      if (parser->max_element_size &&
          result.rest - Parser::buffer.data() - offset > parser->max_element_size) {
        error_msg = "element too big";
      } else {
        auto name_tag = node_name(doc.impl.first_node());
        parser->stream_tag =
          ustring{std::get<0>(name_tag), std::get<1>(name_tag)};
        element = make_stream_start_tuple(ctx, doc.impl.first_node());
      }
    }
  };

  auto hasStreamReopen = [&] {
    auto parseOpenRes =
        doc.parse<parse_open_only()>(Parser::buffer.data() + offset);
    if (parseOpenRes.has_error)
      return false;
    auto tag_name = node_name(doc.impl.first_node());
    return ustring{std::get<0>(tag_name), std::get<1>(tag_name)} ==
           parser->stream_tag;
  };

  auto parseElement = [&] {
    result = doc.parse<parse_one()>(Parser::buffer.data() + offset);
    if (!result.has_error) {
      if (parser->max_element_size &&
        result.rest - Parser::buffer.data() - offset > parser->max_element_size) {
        error_msg = "element too big";
      } else {
        element = make_xmlel(ctx, doc.impl.first_node());
      }
    }
  };

  if (parser->infinite_stream) {
    parseElement();
  } else if (parser->stream_tag.empty()) {
    parseStreamOpen();
  } else if (has_stream_closing_tag(parser, offset)) {
    doc.clear();
    // no data after closing tag
    result.rest = &*Parser::buffer.rbegin();
    element = make_stream_end_tuple(ctx);
  } else {
    parseElement();
  }

  if (result.eof && hasStreamReopen()) {
    doc.clear();
    parseStreamOpen();
  }

  if (result.eof) {
    // Return an error if an incomplete element has at least max_element_size characters.
    if (parser->max_element_size &&
        Parser::buffer.size() - offset > parser->max_element_size) {
      error_msg = "element too big";
    } else {
      result.rest = Parser::buffer.data() + offset;
      element = atom_undefined;
    }
  } else if (result.has_error) {
    error_msg = result.error_message.c_str();
  }

  if (!error_msg) {
    // Return an error when null character is found.
    std::size_t rest_size = &Parser::buffer.back() - result.rest;
    if (std::strlen(reinterpret_cast<const char*>(result.rest)) != rest_size)
      error_msg = "null character found in buffer";
  }

  if (error_msg) {
    return enif_make_tuple2(
        env, atom_error,
        enif_make_string(env, error_msg, ERL_NIF_LATIN1));
  }

  return enif_make_tuple3(
      env, atom_ok, element,
      enif_make_uint64(env, result.rest - Parser::buffer.data()));
}

static ERL_NIF_TERM parse(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  Parser parser;
  parser.copy_buffer(env, argv[0]);
  Parser::term_buffer.clear();

  auto &doc = get_static_doc();

  ParseCtx ctx{env, &parser};
  auto result = doc.parse<default_parse_flags()>(Parser::buffer.data());

  if (!result.has_error) {
    ERL_NIF_TERM element = make_xmlel(ctx, doc.impl.first_node());
    return enif_make_tuple2(env, atom_ok, element);
  }

  return enif_make_tuple2(
      env, atom_error,
      enif_make_string(env, result.error_message.c_str(), ERL_NIF_LATIN1));
}

static ERL_NIF_TERM escape_cdata(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  ErlNifBinary bin;
  if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
    return enif_make_badarg(env);

  rapidxml::xml_node<unsigned char> node(rapidxml::node_data);
  node.value(bin.data, bin.size);
  return node_to_binary(env, node, rapidxml::print_no_indenting);
}

static ERL_NIF_TERM to_binary(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
  int arity;
  const ERL_NIF_TERM *xmlel;
  if (!enif_get_tuple(env, argv[0], &arity, &xmlel))
    return enif_make_badarg(env);

  if (arity != 4 || enif_compare(atom_xmlel, xmlel[0]) != 0)
    return enif_make_badarg(env);

  int flags = rapidxml::print_no_indenting;
  if (enif_compare(atom_pretty, argv[1]) == 0)
    flags = 0;

  xml_document &doc = get_static_doc();
  if (!build_el(env, doc, xmlel, doc.impl))
    return enif_make_badarg(env);

  return node_to_binary(env, doc.impl, flags);
}

static ERL_NIF_TERM reset_parser(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  Parser *parser;
  if (!enif_get_resource(env, argv[0], parser_type,
                         reinterpret_cast<void **>(&parser)))
    return enif_make_badarg(env);

  parser->reset();
  return atom_ok;
}

static ErlNifFunc nif_funcs[] = {
    {"create", 2, create},         {"parse", 1, parse},
    {"parse_next", 2, parse_next}, {"escape_cdata", 1, escape_cdata},
    {"to_binary", 2, to_binary},   {"reset_parser", 1, reset_parser}};
}

ERL_NIF_INIT(exml_nif, nif_funcs, &load, nullptr, nullptr, &unload)
