#ifndef RAPIDXML_PRINT_HPP_INCLUDED
#define RAPIDXML_PRINT_HPP_INCLUDED

// Copyright (C) 2006, 2009 Marcin Kalicinski
// Version 1.13
// Revision $DateTime: 2009/05/13 01:46:17 $
//! \file rapidxml_print.hpp This file contains rapidxml printer implementation

#include "rapidxml.hpp"

// Only include streams if not disabled
#ifndef RAPIDXML_NO_STREAMS
    #include <ostream>
#endif

#include <cstring>
#include <memory>

namespace rapidxml
{

    ///////////////////////////////////////////////////////////////////////
    // PrintBuffer: bulk-write output buffer with pluggable allocator

    template <class Ch, class Alloc = std::allocator<Ch>, std::size_t InitCap = 0>
    class PrintBuffer {
        Alloc m_alloc;
        Ch *m_data;
        std::size_t m_size;
        std::size_t m_capacity;

        void grow(std::size_t needed) {
            std::size_t cap = m_capacity ? m_capacity : 64;
            while (cap < needed) cap *= 2;
            Ch *new_data = std::allocator_traits<Alloc>::allocate(m_alloc, cap);
            if (m_data) {
                std::memcpy(new_data, m_data, m_size * sizeof(Ch));
                std::allocator_traits<Alloc>::deallocate(m_alloc, m_data, m_capacity);
            }
            m_data = new_data;
            m_capacity = cap;
        }

        void init() {
            m_size = 0;
            if (InitCap > 0) {
                m_data = std::allocator_traits<Alloc>::allocate(m_alloc, InitCap);
                m_capacity = InitCap;
            } else {
                m_data = nullptr;
                m_capacity = 0;
            }
        }

    public:
        PrintBuffer() : m_alloc() {
            init();
        }
        explicit PrintBuffer(const Alloc &alloc) : m_alloc(alloc) {
            init();
        }
        ~PrintBuffer() {
            if (m_data)
                std::allocator_traits<Alloc>::deallocate(m_alloc, m_data, m_capacity);
        }

        PrintBuffer(const PrintBuffer &) = delete;
        PrintBuffer &operator=(const PrintBuffer &) = delete;
        PrintBuffer(PrintBuffer &&o) noexcept
            : m_alloc(std::move(o.m_alloc)), m_data(o.m_data),
              m_size(o.m_size), m_capacity(o.m_capacity) {
            o.m_data = nullptr; o.m_size = 0; o.m_capacity = 0;
        }
        PrintBuffer &operator=(PrintBuffer &&o) noexcept {
            if (this != &o) {
                if (m_data) std::allocator_traits<Alloc>::deallocate(m_alloc, m_data, m_capacity);
                m_alloc = std::move(o.m_alloc);
                m_data = o.m_data; m_size = o.m_size; m_capacity = o.m_capacity;
                o.m_data = nullptr; o.m_size = 0; o.m_capacity = 0;
            }
            return *this;
        }

        void clear() { m_size = 0; }
        const Ch *data() const { return m_data; }
        std::size_t size() const { return m_size; }

        inline void ensure(std::size_t additional) {
            if (__builtin_expect(m_size + additional <= m_capacity, 1)) return;
            grow(m_size + additional);
        }
        void append(const Ch *src, std::size_t n) {
            ensure(n);
            std::memcpy(m_data + m_size, src, n * sizeof(Ch));
            m_size += n;
        }
        void append_char(Ch ch) {
            ensure(1);
            m_data[m_size++] = ch;
        }
        void append_fill(Ch ch, std::size_t n) {
            ensure(n);
            std::memset(m_data + m_size, static_cast<unsigned char>(ch), n);
            m_size += n;
        }
        Ch *reserve_raw(std::size_t n) {
            ensure(n);
            return m_data + m_size;
        }
        void advance(std::size_t n) { m_size += n; }
    };

    ///////////////////////////////////////////////////////////////////////
    // Printing flags

    const int print_no_indenting = 0x1;   //!< Printer flag instructing the printer to suppress indenting of XML. See print() function.

    ///////////////////////////////////////////////////////////////////////
    // Internal

    //! \cond internal
    namespace internal
    {

        ///////////////////////////////////////////////////////////////////////////
        // Internal character operations

        // Find character
        template<class Ch, Ch ch>
        inline bool find_char(const Ch *begin, const Ch *end)
        {
            static_assert(sizeof(Ch) == 1, "SIMD print path requires byte-sized Ch");
            return std::memchr(begin, static_cast<unsigned char>(ch),
                               static_cast<std::size_t>(end - begin)) != nullptr;
        }

        ///////////////////////////////////////////////////////////////////////////
        // Internal printing (PrintBuffer: chunk ensure + raw writes)

        // Expand characters into entity references, writing directly into buf
        template<class Ch, class Alloc, std::size_t N>
        inline void expand_chars(PrintBuffer<Ch, Alloc, N> &buf,
                                     const Ch *begin, const Ch *end, Ch noexpand)
        {
            static_assert(sizeof(Ch) == 1, "SIMD print path requires byte-sized Ch");
            const std::size_t len = static_cast<std::size_t>(end - begin);
            Ch *p = buf.reserve_raw(len * 6);  // worst case: all &apos;
            Ch *start = p;
            while (begin != end)
            {
                if (*begin == noexpand)
                {
                    *p++ = *begin;
                }
                else
                {
                    switch (*begin)
                    {
                    case Ch('<'):
                        std::memcpy(p, "&lt;", 4); p += 4;
                        break;
                    case Ch('>'):
                        std::memcpy(p, "&gt;", 4); p += 4;
                        break;
                    case Ch('\''):
                        std::memcpy(p, "&apos;", 6); p += 6;
                        break;
                    case Ch('"'):
                        std::memcpy(p, "&quot;", 6); p += 6;
                        break;
                    case Ch('&'):
                        std::memcpy(p, "&amp;", 5); p += 5;
                        break;
                    default:
                        *p++ = *begin;
                    }
                }
                ++begin;
            }
            buf.advance(static_cast<std::size_t>(p - start));
        }

        // Forward declaration
        template<class Ch, class Alloc, std::size_t N>
        inline void print_node(PrintBuffer<Ch, Alloc, N> &buf,
                                   const xml_node<Ch> *node, int flags, int indent);

        template<class Ch, class Alloc, std::size_t N>
        inline void print_children(PrintBuffer<Ch, Alloc, N> &buf,
                                       const xml_node<Ch> *node, int flags, int indent)
        {
            for (xml_node<Ch> *child = node->first_node(); child; child = child->next_sibling())
                print_node(buf, child, flags, indent);
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_attributes(PrintBuffer<Ch, Alloc, N> &buf,
                                         const xml_node<Ch> *node, int)
        {
            for (xml_attribute<Ch> *attr = node->first_attribute(); attr; attr = attr->next_attribute())
            {
                if (attr->name() && attr->value())
                {
                    const Ch *aname = attr->name();
                    const std::size_t aname_size = attr->name_size();
                    const Ch *avalue = attr->value();
                    const std::size_t avalue_size = attr->value_size();

                    if (find_char<Ch, Ch('\'')>(avalue, avalue + avalue_size))
                    {
                        // ' name="...escaped..."'
                        Ch *p = buf.reserve_raw(1 + aname_size + 2);
                        *p++ = Ch(' ');
                        std::memcpy(p, aname, aname_size); p += aname_size;
                        std::memcpy(p, "=\"", 2); p += 2;
                        buf.advance(1 + aname_size + 2);
                        expand_chars(buf, avalue, avalue + avalue_size, Ch('\''));
                        buf.append_char(Ch('"'));
                    }
                    else
                    {
                        // " name='...escaped...'"
                        Ch *p = buf.reserve_raw(1 + aname_size + 2);
                        *p++ = Ch(' ');
                        std::memcpy(p, aname, aname_size); p += aname_size;
                        std::memcpy(p, "=\'", 2); p += 2;
                        buf.advance(1 + aname_size + 2);
                        expand_chars(buf, avalue, avalue + avalue_size, Ch('"'));
                        buf.append_char(Ch('\''));
                    }
                }
            }
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_data_node(PrintBuffer<Ch, Alloc, N> &buf,
                                        const xml_node<Ch> *node, int flags, int indent)
        {
            if (!(flags & print_no_indenting))
                buf.append_fill(Ch(' '), indent);
            expand_chars(buf, node->value(), node->value() + node->value_size(), Ch(0));
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_cdata_node(PrintBuffer<Ch, Alloc, N> &buf,
                                         const xml_node<Ch> *node, int flags, int indent)
        {
            if (!(flags & print_no_indenting))
                buf.append_fill(Ch(' '), indent);
            // Single ensure for: <![CDATA[ + value + ]]>
            const std::size_t val_size = node->value_size();
            Ch *p = buf.reserve_raw(9 + val_size + 3);
            std::memcpy(p, "<![CDATA[", 9); p += 9;
            std::memcpy(p, node->value(), val_size); p += val_size;
            std::memcpy(p, "]]>", 3);
            buf.advance(9 + val_size + 3);
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_element_node(PrintBuffer<Ch, Alloc, N> &buf,
                                           const xml_node<Ch> *node, int flags, int indent)
        {
            const Ch *name = node->name();
            const std::size_t name_size = node->name_size();

            if (!(flags & print_no_indenting))
                buf.append_fill(Ch(' '), indent);

            // Opening: '<' + name
            {
                Ch *p = buf.reserve_raw(1 + name_size);
                *p = Ch('<');
                std::memcpy(p + 1, name, name_size);
                buf.advance(1 + name_size);
            }

            print_attributes(buf, node, flags);

            if (node->value_size() == 0 && !node->first_node())
            {
                // Self-closing: '/>'
                Ch *p = buf.reserve_raw(2);
                std::memcpy(p, "/>", 2);
                buf.advance(2);
            }
            else
            {
                buf.append_char(Ch('>'));

                xml_node<Ch> *child = node->first_node();
                if (!child)
                {
                    expand_chars(buf, node->value(), node->value() + node->value_size(), Ch(0));
                }
                else if (child->next_sibling() == 0 && child->type() == node_data)
                {
                    expand_chars(buf, child->value(), child->value() + child->value_size(), Ch(0));
                }
                else
                {
                    if (!(flags & print_no_indenting))
                        buf.append_char(Ch('\n'));
                    print_children(buf, node, flags, indent + 2);
                    if (!(flags & print_no_indenting))
                        buf.append_fill(Ch(' '), indent);
                }

                // Closing: '</' + name + '>'
                {
                    const std::size_t total = 2 + name_size + 1;
                    Ch *p = buf.reserve_raw(total);
                    std::memcpy(p, "</", 2);
                    std::memcpy(p + 2, name, name_size);
                    p[2 + name_size] = Ch('>');
                    buf.advance(total);
                }
            }
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_declaration_node(PrintBuffer<Ch, Alloc, N> &buf,
                                               const xml_node<Ch> *node, int flags, int indent)
        {
            if (!(flags & print_no_indenting))
                buf.append_fill(Ch(' '), indent);
            buf.append(reinterpret_cast<const Ch *>("<?xml"), 5);
            print_attributes(buf, node, flags);
            Ch *p = buf.reserve_raw(2);
            std::memcpy(p, "?>", 2);
            buf.advance(2);
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_comment_node(PrintBuffer<Ch, Alloc, N> &buf,
                                           const xml_node<Ch> *node, int flags, int indent)
        {
            if (!(flags & print_no_indenting))
                buf.append_fill(Ch(' '), indent);
            // Single ensure for: <!-- + value + -->
            const std::size_t val_size = node->value_size();
            Ch *p = buf.reserve_raw(4 + val_size + 3);
            std::memcpy(p, "<!--", 4);
            p += 4;
            std::memcpy(p, node->value(), val_size);
            p += val_size;
            std::memcpy(p, "-->", 3);
            buf.advance(4 + val_size + 3);
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_doctype_node(PrintBuffer<Ch, Alloc, N> &buf,
                                           const xml_node<Ch> *node, int flags, int indent)
        {
            if (!(flags & print_no_indenting))
                buf.append_fill(Ch(' '), indent);
            // Single ensure for: <!DOCTYPE  + value + >
            const std::size_t val_size = node->value_size();
            Ch *p = buf.reserve_raw(10 + val_size + 1);
            std::memcpy(p, "<!DOCTYPE ", 10);
            p += 10;
            std::memcpy(p, node->value(), val_size);
            p += val_size;
            *p = Ch('>');
            buf.advance(10 + val_size + 1);
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_pi_node(PrintBuffer<Ch, Alloc, N> &buf,
                                      const xml_node<Ch> *node, int flags, int indent)
        {
            if (!(flags & print_no_indenting))
                buf.append_fill(Ch(' '), indent);
            // Single ensure for: <? + name + ' ' + value + ?>
            const std::size_t name_size = node->name_size();
            const std::size_t val_size = node->value_size();
            const std::size_t total = 2 + name_size + 1 + val_size + 2;
            Ch *p = buf.reserve_raw(total);
            std::memcpy(p, "<?", 2);
            p += 2;
            std::memcpy(p, node->name(), name_size);
            p += name_size;
            *p++ = Ch(' ');
            std::memcpy(p, node->value(), val_size);
            p += val_size;
            std::memcpy(p, "?>", 2);
            buf.advance(total);
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_literal_node(PrintBuffer<Ch, Alloc, N> &buf,
                                           const xml_node<Ch> *node, int flags, int indent)
        {
            if (!(flags & print_no_indenting))
                buf.append_fill(Ch(' '), indent);
            buf.append(node->value(), node->value_size());
        }

        template<class Ch, class Alloc, std::size_t N>
        inline void print_node(PrintBuffer<Ch, Alloc, N> &buf,
                                   const xml_node<Ch> *node, int flags, int indent)
        {
            switch (node->type())
            {
            case node_document:
                print_children(buf, node, flags, indent);
                break;
            case node_element:
                print_element_node(buf, node, flags, indent);
                break;
            case node_data:
                print_data_node(buf, node, flags, indent);
                break;
            case node_cdata:
                print_cdata_node(buf, node, flags, indent);
                break;
            case node_declaration:
                print_declaration_node(buf, node, flags, indent);
                break;
            case node_comment:
                print_comment_node(buf, node, flags, indent);
                break;
            case node_doctype:
                print_doctype_node(buf, node, flags, indent);
                break;
            case node_pi:
                print_pi_node(buf, node, flags, indent);
                break;
            case node_literal:
                print_literal_node(buf, node, flags, indent);
                break;
            default:
                assert(0);
                break;
            }

            if (!(flags & print_no_indenting))
                buf.append_char(Ch('\n'));
        }

    }
    //! \endcond

    ///////////////////////////////////////////////////////////////////////////
    // Printing

    //! Prints XML into a PrintBuffer.
    template<class Ch, class Alloc, std::size_t N>
    inline void print(PrintBuffer<Ch, Alloc, N> &buf, const xml_node<Ch> &node, int flags = 0)
    {
        internal::print_node(buf, &node, flags, 0);
    }

#ifndef RAPIDXML_NO_STREAMS

    //! Prints XML to given output stream.
    //! \param out Output stream to print to.
    //! \param node Node to be printed. Pass xml_document to print entire document.
    //! \param flags Flags controlling how XML is printed.
    //! \return Output stream.
    template<class Ch>
    inline std::basic_ostream<Ch> &print(std::basic_ostream<Ch> &out, const xml_node<Ch> &node, int flags = 0)
    {
        PrintBuffer<Ch> buf;
        internal::print_node(buf, &node, flags, 0);
        out.write(buf.data(), static_cast<std::streamsize>(buf.size()));
        return out;
    }

    //! Prints formatted XML to given output stream. Uses default printing flags. Use print() function to customize printing process.
    //! \param out Output stream to print to.
    //! \param node Node to be printed.
    //! \return Output stream.
    template<class Ch>
    inline std::basic_ostream<Ch> &operator <<(std::basic_ostream<Ch> &out, const xml_node<Ch> &node)
    {
        return print(out, node);
    }

#endif

}

#endif
