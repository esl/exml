// SIMD fast-path for rapidxml::xml_document::skip() — 128-bit lanes (SSE2/NEON via SIMDe).
// Portable across x86_64 (SSE2) and AArch64 (NEON). Requires buffer padding
// (see Parser::kSimdTailPadding in exml.cpp) of at least 15 bytes after the null terminator.

#ifndef EXML_SIMD_SKIP_HPP
#define EXML_SIMD_SKIP_HPP

#include <cstddef>
#include <cstdint>

#include "simde/x86/sse2.h"

// Included at global scope after namespace rapidxml { ... } so SIMDe headers see ::math, not rapidxml::.

namespace rapidxml {

// XML 1.0 Char — matches internal::lookup_tables::lookup_control_points
inline bool xml_char_allowed_xml10(unsigned char b) {
  if (b >= 0x20)
    return true;
  if (b == 0x09 || b == 0x0A || b == 0x0D)
    return true;
  return false;
}

inline unsigned first_mmask16_index(std::uint16_t m) {
  return static_cast<unsigned>(__builtin_ctz(static_cast<unsigned>(m)));
}

// Bitmask (16 lanes) where Char is violated: unsigned byte < 0x20 and not TAB/LF/CR.
inline std::uint16_t simd_forbidden_control_mask(simde__m128i chunk) {
  const simde__m128i v_zero = simde_mm_setzero_si128();
  const simde__m128i v_hi = simde_mm_set1_epi8(static_cast<char>(0x80));
  const simde__m128i v_20 = simde_mm_set1_epi8(static_cast<char>(0x20));
  const simde__m128i v_tab = simde_mm_set1_epi8(0x09);
  const simde__m128i v_nl = simde_mm_set1_epi8(0x0A);
  const simde__m128i v_cr = simde_mm_set1_epi8(0x0D);
  const simde__m128i masked = simde_mm_and_si128(chunk, v_hi);
  const simde__m128i ascii = simde_mm_cmpeq_epi8(masked, v_zero);
  const simde__m128i below = simde_mm_cmpgt_epi8(v_20, chunk);
  const simde__m128i below_space = simde_mm_and_si128(below, ascii);
  const simde__m128i tab = simde_mm_cmpeq_epi8(chunk, v_tab);
  const simde__m128i nl = simde_mm_cmpeq_epi8(chunk, v_nl);
  const simde__m128i cr = simde_mm_cmpeq_epi8(chunk, v_cr);
  const simde__m128i allowed = simde_mm_or_si128(tab, simde_mm_or_si128(nl, cr));
  const simde__m128i bad = simde_mm_andnot_si128(allowed, below_space);
  return static_cast<std::uint16_t>(simde_mm_movemask_epi8(bad));
}

// Uses already-loaded `chunk` at `text` — avoids a second loadu on the stop path, and uses a
// cheap scalar prefix check for short stops (common: e.g. first byte is `<`).
template<int Flags>
inline bool simd_prefix_control_error_from_chunk(simde__m128i chunk, unsigned first_stop_idx,
                                                 unsigned char *&text) {
  if (!(Flags & parse_validate_control_chars))
    return false;
  if (first_stop_idx <= 4) {
    for (unsigned i = 0; i < first_stop_idx; ++i) {
      if (!xml_char_allowed_xml10(text[i])) {
        text += i;
        return true;
      }
    }
    return false;
  }
  std::uint16_t bad = simd_forbidden_control_mask(chunk);
  const std::uint16_t prefix =
      first_stop_idx >= 16 ? static_cast<std::uint16_t>(0xFFFFu)
                           : static_cast<std::uint16_t>((1u << first_stop_idx) - 1u);
  bad &= prefix;
  if (!bad)
    return false;
  text += first_mmask16_index(bad);
  return true;
}

template<int Flags, typename FStop, typename FScalarTest>
inline void simd_skip_with_stops(unsigned char *&text, FStop &&stop_mask_for_chunk,
                                 FScalarTest &&scalar_test) {
  // Scalar prefix: handle short runs without touching SIMD registers.
  // Most skip calls in structured XML (names, whitespace between tags, short attr values)
  // stop within a few bytes.  Paying SIMD setup for 1-3 byte advances is a net loss.
  {
    unsigned char *p = text;
    const unsigned char *limit = text + 16;  // at most one chunk worth
    while (p < limit && scalar_test(*p)) {
      if (Flags & parse_validate_control_chars) {
        if (!xml_char_allowed_xml10(*p)) {
          text = p;
          return;
        }
      }
      ++p;
    }
    if (p < limit) {
      // Found stop byte within the first 16 bytes — done without SIMD.
      text = p;
      return;
    }
    text = p;
  }

  // SIMD loop for longer runs.
  const simde__m128i v_zero = simde_mm_setzero_si128();
  while (true) {
    simde__m128i chunk = simde_mm_loadu_si128(reinterpret_cast<const simde__m128i *>(text));
    std::uint16_t mask = stop_mask_for_chunk(chunk, v_zero);
    if (mask) {
      const unsigned first_stop = first_mmask16_index(mask);
      if (simd_prefix_control_error_from_chunk<Flags>(chunk, first_stop, text))
        return;
      text += first_stop;
      return;
    }
    if (Flags & parse_validate_control_chars) {
      std::uint16_t bad_ctrl = simd_forbidden_control_mask(chunk);
      if (bad_ctrl) {
        text += first_mmask16_index(bad_ctrl);
        return;
      }
    }
    text += 16;
  }
}

template<int Flags>
inline void simd_advance_text_pred(unsigned char *&text) {
  const simde__m128i v_lt = simde_mm_set1_epi8(static_cast<char>('<'));
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i vz) {
    const simde__m128i hit = simde_mm_or_si128(
        simde_mm_cmpeq_epi8(chunk, vz),
        simde_mm_cmpeq_epi8(chunk, v_lt));
    return static_cast<std::uint16_t>(simde_mm_movemask_epi8(hit));
  }, [](unsigned char b) { return b != 0 && b != '<'; });
}

template<int Flags>
inline void simd_advance_text_pure_no_ws(unsigned char *&text) {
  const simde__m128i v_lt = simde_mm_set1_epi8(static_cast<char>('<'));
  const simde__m128i v_amp = simde_mm_set1_epi8(static_cast<char>('&'));
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i vz) {
    const simde__m128i hit = simde_mm_or_si128(
        simde_mm_cmpeq_epi8(chunk, vz),
        simde_mm_or_si128(
            simde_mm_cmpeq_epi8(chunk, v_lt),
            simde_mm_cmpeq_epi8(chunk, v_amp)));
    return static_cast<std::uint16_t>(simde_mm_movemask_epi8(hit));
  }, [](unsigned char b) { return b != 0 && b != '<' && b != '&'; });
}

// Matches lookup_text_pure_with_ws (stops at NUL, TAB, LF, CR, space, &, <).
// Uses range check: all bytes <= 0x20 covers NUL/TAB/LF/CR/space, then add & and <.
inline std::uint16_t simd_mask_stop_text_pure_with_ws(simde__m128i chunk) {
  const simde__m128i v_hi = simde_mm_set1_epi8(static_cast<char>(0x80));
  const simde__m128i v_21 = simde_mm_set1_epi8(static_cast<char>(0x21));
  const simde__m128i v_zero = simde_mm_setzero_si128();
  // Unsigned <= 0x20: mask high bit to treat as signed, then signed cmpgt(0x21, chunk)
  const simde__m128i ascii = simde_mm_cmpeq_epi8(simde_mm_and_si128(chunk, v_hi), v_zero);
  const simde__m128i le_space = simde_mm_and_si128(simde_mm_cmpgt_epi8(v_21, chunk), ascii);
  const simde__m128i amp = simde_mm_cmpeq_epi8(chunk, simde_mm_set1_epi8(static_cast<char>('&')));
  const simde__m128i lt = simde_mm_cmpeq_epi8(chunk, simde_mm_set1_epi8(static_cast<char>('<')));
  const simde__m128i hit = simde_mm_or_si128(le_space, simde_mm_or_si128(amp, lt));
  return static_cast<std::uint16_t>(simde_mm_movemask_epi8(hit));
}

template<int Flags>
inline void simd_advance_text_pure_with_ws(unsigned char *&text) {
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i) {
    return simd_mask_stop_text_pure_with_ws(chunk);
  }, [](unsigned char b) { return b > 0x20 && b != '&' && b != '<'; });
}

// Attribute value in double quotes: lookup_attribute_data_2 — stop at NUL or " only (& is data).
template<int Flags>
inline void simd_advance_attr_dquote_full(unsigned char *&text) {
  const simde__m128i v_quot = simde_mm_set1_epi8(static_cast<char>('"'));
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i vz) {
    const simde__m128i hit = simde_mm_or_si128(
        simde_mm_cmpeq_epi8(chunk, vz),
        simde_mm_cmpeq_epi8(chunk, v_quot));
    return static_cast<std::uint16_t>(simde_mm_movemask_epi8(hit));
  }, [](unsigned char b) { return b != 0 && b != '"'; });
}

// Attribute value in single quotes: lookup_attribute_data_1 — stop at NUL or ' only.
template<int Flags>
inline void simd_advance_attr_squote_full(unsigned char *&text) {
  const simde__m128i v_squot = simde_mm_set1_epi8(static_cast<char>('\''));
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i vz) {
    const simde__m128i hit = simde_mm_or_si128(
        simde_mm_cmpeq_epi8(chunk, vz),
        simde_mm_cmpeq_epi8(chunk, v_squot));
    return static_cast<std::uint16_t>(simde_mm_movemask_epi8(hit));
  }, [](unsigned char b) { return b != 0 && b != '\''; });
}

template<int Flags>
inline void simd_advance_attr_dquote_pure(unsigned char *&text) {
  const simde__m128i v_quot = simde_mm_set1_epi8(static_cast<char>('"'));
  const simde__m128i v_amp = simde_mm_set1_epi8(static_cast<char>('&'));
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i vz) {
    const simde__m128i hit = simde_mm_or_si128(
        simde_mm_cmpeq_epi8(chunk, vz),
        simde_mm_or_si128(
            simde_mm_cmpeq_epi8(chunk, v_quot),
            simde_mm_cmpeq_epi8(chunk, v_amp)));
    return static_cast<std::uint16_t>(simde_mm_movemask_epi8(hit));
  }, [](unsigned char b) { return b != 0 && b != '"' && b != '&'; });
}

template<int Flags>
inline void simd_advance_attr_squote_pure(unsigned char *&text) {
  const simde__m128i v_squot = simde_mm_set1_epi8(static_cast<char>('\''));
  const simde__m128i v_amp = simde_mm_set1_epi8(static_cast<char>('&'));
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i vz) {
    const simde__m128i hit = simde_mm_or_si128(
        simde_mm_cmpeq_epi8(chunk, vz),
        simde_mm_or_si128(
            simde_mm_cmpeq_epi8(chunk, v_squot),
            simde_mm_cmpeq_epi8(chunk, v_amp)));
    return static_cast<std::uint16_t>(simde_mm_movemask_epi8(hit));
  }, [](unsigned char b) { return b != 0 && b != '\'' && b != '&'; });
}

// Matches lookup_whitespace: space, tab, LF, CR.
inline simde__m128i simd_vec_whitespace(simde__m128i chunk) {
  const simde__m128i v_sp = simde_mm_set1_epi8(static_cast<char>(0x20));
  const simde__m128i v_tab = simde_mm_set1_epi8(static_cast<char>(0x09));
  const simde__m128i v_nl = simde_mm_set1_epi8(static_cast<char>(0x0A));
  const simde__m128i v_cr = simde_mm_set1_epi8(static_cast<char>(0x0D));
  simde__m128i m = simde_mm_or_si128(simde_mm_cmpeq_epi8(chunk, v_sp), simde_mm_cmpeq_epi8(chunk, v_tab));
  m = simde_mm_or_si128(m, simde_mm_cmpeq_epi8(chunk, v_nl));
  m = simde_mm_or_si128(m, simde_mm_cmpeq_epi8(chunk, v_cr));
  return m;
}

inline std::uint16_t simd_mask_whitespace(simde__m128i chunk) {
  return static_cast<std::uint16_t>(simde_mm_movemask_epi8(simd_vec_whitespace(chunk)));
}

// Helper: OR-reduce a variadic list of cmpeq results into a single vector, then movemask once.
inline simde__m128i simd_eq1(simde__m128i chunk, unsigned char c) {
  return simde_mm_cmpeq_epi8(chunk, simde_mm_set1_epi8(static_cast<char>(c)));
}

inline simde__m128i simd_or_eq(simde__m128i chunk, unsigned char c) {
  return simd_eq1(chunk, c);
}

template<typename... Chars>
inline simde__m128i simd_or_eq(simde__m128i chunk, unsigned char c, Chars... rest) {
  return simde_mm_or_si128(simd_eq1(chunk, c), simd_or_eq(chunk, rest...));
}

// Stops match lookup_node_name (0 bytes): NUL, TAB, LF, CR, space, /, >, ?
inline std::uint16_t simd_mask_stop_node_name(simde__m128i chunk) {
  return static_cast<std::uint16_t>(simde_mm_movemask_epi8(
      simd_or_eq(chunk, 0x00, 0x09, 0x0A, 0x0D, 0x20, 0x2F, 0x3E, 0x3F)));
}

// Stops match lookup_element_name: node_name stops + ':'
inline std::uint16_t simd_mask_stop_element_name(simde__m128i chunk) {
  return static_cast<std::uint16_t>(simde_mm_movemask_epi8(
      simd_or_eq(chunk, 0x00, 0x09, 0x0A, 0x0D, 0x20, 0x2F, 0x3A, 0x3E, 0x3F)));
}

// Stops match lookup_attribute_name: NUL, TAB, LF, CR, space, !, /, <, =, >, ?
inline std::uint16_t simd_mask_stop_attribute_name(simde__m128i chunk) {
  return static_cast<std::uint16_t>(simde_mm_movemask_epi8(
      simd_or_eq(chunk, 0x00, 0x09, 0x0A, 0x0D, 0x20, 0x21, 0x2F, 0x3C, 0x3D, 0x3E, 0x3F)));
}

template<int Flags>
inline void simd_advance_whitespace(unsigned char *&text) {
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i) {
    const std::uint16_t ws = simd_mask_whitespace(chunk);
    return static_cast<std::uint16_t>(~ws & 0xFFFFu);
  }, [](unsigned char b) { return b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D; });
}

template<int Flags>
inline void simd_advance_node_name(unsigned char *&text) {
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i) {
    return simd_mask_stop_node_name(chunk);
  }, [](unsigned char b) {
    return b != 0 && b != 0x09 && b != 0x0A && b != 0x0D && b != ' ' && b != '/' && b != '>' && b != '?';
  });
}

template<int Flags>
inline void simd_advance_element_name(unsigned char *&text) {
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i) {
    return simd_mask_stop_element_name(chunk);
  }, [](unsigned char b) {
    return b != 0 && b != 0x09 && b != 0x0A && b != 0x0D && b != ' ' && b != '/' && b != ':' && b != '>' && b != '?';
  });
}

template<int Flags>
inline void simd_advance_attribute_name(unsigned char *&text) {
  simd_skip_with_stops<Flags>(text, [&](simde__m128i chunk, simde__m128i) {
    return simd_mask_stop_attribute_name(chunk);
  }, [](unsigned char b) {
    return b != 0 && b != 0x09 && b != 0x0A && b != 0x0D && b != ' ' && b != '!' && b != '/' && b != '<' && b != '=' && b != '>' && b != '?';
  });
}

// Trims trailing XML whitespace (TAB/LF/CR/space). Call only when sizeof(Ch)==1.
template<typename Ch>
inline void simd_trim_trailing_whitespace(Ch *&end, Ch *value_start) {
  auto *e = reinterpret_cast<unsigned char *>(end);
  const auto *vs = reinterpret_cast<const unsigned char *>(value_start);
  while (e > vs) {
    const std::size_t len = static_cast<std::size_t>(e - vs);
    if (len < 16) {
      while (e > vs) {
        const unsigned char b = e[-1];
        const bool ws = (b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D);
        if (!ws)
          break;
        --e;
      }
      end = reinterpret_cast<Ch *>(e);
      return;
    }
    unsigned char *const base = e - 16;
    const simde__m128i chunk = simde_mm_loadu_si128(reinterpret_cast<const simde__m128i *>(base));
    const std::uint16_t ws = simd_mask_whitespace(chunk);
    const std::uint16_t nz = static_cast<std::uint16_t>(~ws & 0xFFFFu);
    if (nz == 0) {
      e = base;
      continue;
    }
    // Highest set bit position in [0..15]. nz is zero-extended to 32-bit for clz.
    const unsigned hi = static_cast<unsigned>(31 - __builtin_clz(static_cast<unsigned>(nz)));
    e = base + hi + 1;
    end = reinterpret_cast<Ch *>(e);
    return;
  }
  end = reinterpret_cast<Ch *>(e);
}

// --- Delimiter scans (16-byte SIMD validation + candidate search; sizeof(Ch)==1) ----------

template<int Flags, typename Ch>
inline void simd_scan_until_pi_value_end(Ch *&text) {
  if (sizeof(Ch) != 1) {
    while (text[0] != Ch('?') || text[1] != Ch('>')) {
      if (*text == Ch('\0'))
        RAPIDXML_PARSE_ERROR("unexpected end of data", text);
      ++text;
    }
    return;
  }
  unsigned char *p = reinterpret_cast<unsigned char *>(text);
  const simde__m128i v_zero = simde_mm_setzero_si128();
  const simde__m128i v_q = simde_mm_set1_epi8(static_cast<char>('?'));
  while (true) {
    const simde__m128i chunk = simde_mm_loadu_si128(reinterpret_cast<const simde__m128i *>(p));
    std::uint16_t qm =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_q)));
    while (qm) {
      const unsigned i = first_mmask16_index(qm);
      if (p[i + 1] == static_cast<unsigned char>('>')) {
        text = reinterpret_cast<Ch *>(p + i);
        return;
      }
      qm = static_cast<std::uint16_t>(qm & (qm - 1u));
    }
    const std::uint16_t z =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_zero)));
    if (z)
      RAPIDXML_PARSE_ERROR("unexpected end of data",
                           reinterpret_cast<Ch *>(p + first_mmask16_index(z)));
    p += 16;
  }
}

template<int Flags, typename Ch>
inline void simd_scan_until_xml_declaration_skip_end(Ch *&text) {
  if (sizeof(Ch) != 1) {
    while (text[0] != Ch('?') || text[1] != Ch('>')) {
      if (!text[0])
        RAPIDXML_PARSE_ERROR("unexpected end of data", text);
      if (Flags & parse_validate_control_chars) {
        if (!internal::lookup_tables<0>::lookup_control_points[static_cast<unsigned char>(*text)])
          RAPIDXML_PARSE_ERROR("unexpected control character", text);
      }
      ++text;
    }
    return;
  }
  unsigned char *p = reinterpret_cast<unsigned char *>(text);
  const simde__m128i v_zero = simde_mm_setzero_si128();
  const simde__m128i v_q = simde_mm_set1_epi8(static_cast<char>('?'));
  while (true) {
    const simde__m128i chunk = simde_mm_loadu_si128(reinterpret_cast<const simde__m128i *>(p));
    std::uint16_t qm =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_q)));
    while (qm) {
      const unsigned i = first_mmask16_index(qm);
      if (p[i + 1] == static_cast<unsigned char>('>')) {
        if (Flags & parse_validate_control_chars) {
          const std::uint16_t bad = simd_forbidden_control_mask(chunk);
          const std::uint16_t prefix = i ? static_cast<std::uint16_t>((1u << i) - 1u) : std::uint16_t{0};
          const std::uint16_t masked = static_cast<std::uint16_t>(bad & prefix);
          if (masked)
            RAPIDXML_PARSE_ERROR("unexpected control character",
                                 reinterpret_cast<Ch *>(p + first_mmask16_index(masked)));
        }
        text = reinterpret_cast<Ch *>(p + i);
        return;
      }
      qm = static_cast<std::uint16_t>(qm & (qm - 1u));
    }
    const std::uint16_t z =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_zero)));
    if (z)
      RAPIDXML_PARSE_ERROR("unexpected end of data",
                           reinterpret_cast<Ch *>(p + first_mmask16_index(z)));
    if (Flags & parse_validate_control_chars) {
      const std::uint16_t bad = simd_forbidden_control_mask(chunk);
      if (bad)
        RAPIDXML_PARSE_ERROR("unexpected control character",
                             reinterpret_cast<Ch *>(p + first_mmask16_index(bad)));
    }
    p += 16;
  }
}

template<int Flags, typename Ch>
inline void simd_scan_until_comment_end(Ch *&text) {
  if (sizeof(Ch) != 1) {
    while (text[0] != Ch('-') || text[1] != Ch('-') || text[2] != Ch('>')) {
      if (!text[0])
        RAPIDXML_PARSE_ERROR("unexpected end of data", text);
      if (Flags & parse_validate_control_chars) {
        if (!internal::lookup_tables<0>::lookup_control_points[static_cast<unsigned char>(*text)])
          RAPIDXML_PARSE_ERROR("unexpected control character", text);
      }
      ++text;
    }
    return;
  }
  unsigned char *p = reinterpret_cast<unsigned char *>(text);
  const simde__m128i v_zero = simde_mm_setzero_si128();
  const simde__m128i v_dash = simde_mm_set1_epi8(static_cast<char>('-'));
  while (true) {
    const simde__m128i chunk = simde_mm_loadu_si128(reinterpret_cast<const simde__m128i *>(p));
    std::uint16_t dash =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_dash)));
    while (dash) {
      const unsigned i = first_mmask16_index(dash);
      if (p[i + 1] == static_cast<unsigned char>('-') && p[i + 2] == static_cast<unsigned char>('>')) {
        if (Flags & parse_validate_control_chars) {
          const std::uint16_t bad = simd_forbidden_control_mask(chunk);
          const std::uint16_t prefix = i ? static_cast<std::uint16_t>((1u << i) - 1u) : std::uint16_t{0};
          const std::uint16_t masked = static_cast<std::uint16_t>(bad & prefix);
          if (masked)
            RAPIDXML_PARSE_ERROR("unexpected control character",
                                 reinterpret_cast<Ch *>(p + first_mmask16_index(masked)));
        }
        text = reinterpret_cast<Ch *>(p + i);
        return;
      }
      dash = static_cast<std::uint16_t>(dash & (dash - 1u));
    }
    const std::uint16_t z =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_zero)));
    if (z)
      RAPIDXML_PARSE_ERROR("unexpected end of data",
                           reinterpret_cast<Ch *>(p + first_mmask16_index(z)));
    if (Flags & parse_validate_control_chars) {
      const std::uint16_t bad = simd_forbidden_control_mask(chunk);
      if (bad)
        RAPIDXML_PARSE_ERROR("unexpected control character",
                             reinterpret_cast<Ch *>(p + first_mmask16_index(bad)));
    }
    p += 16;
  }
}

template<int Flags, typename Ch>
inline void simd_scan_until_cdata_end(Ch *&text) {
  if (sizeof(Ch) != 1) {
    while (text[0] != Ch(']') || text[1] != Ch(']') || text[2] != Ch('>')) {
      if (!text[0])
        RAPIDXML_PARSE_ERROR("unexpected end of data", text);
      if (Flags & parse_validate_control_chars) {
        if (!internal::lookup_tables<0>::lookup_control_points[static_cast<unsigned char>(*text)])
          RAPIDXML_PARSE_ERROR("unexpected control character", text);
      }
      ++text;
    }
    return;
  }
  unsigned char *p = reinterpret_cast<unsigned char *>(text);
  const simde__m128i v_zero = simde_mm_setzero_si128();
  const simde__m128i v_brack = simde_mm_set1_epi8(static_cast<char>(']'));
  while (true) {
    const simde__m128i chunk = simde_mm_loadu_si128(reinterpret_cast<const simde__m128i *>(p));
    std::uint16_t br =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_brack)));
    while (br) {
      const unsigned i = first_mmask16_index(br);
      if (p[i + 1] == static_cast<unsigned char>(']') && p[i + 2] == static_cast<unsigned char>('>')) {
        if (Flags & parse_validate_control_chars) {
          const std::uint16_t bad = simd_forbidden_control_mask(chunk);
          const std::uint16_t prefix = i ? static_cast<std::uint16_t>((1u << i) - 1u) : std::uint16_t{0};
          const std::uint16_t masked = static_cast<std::uint16_t>(bad & prefix);
          if (masked)
            RAPIDXML_PARSE_ERROR("unexpected control character",
                                 reinterpret_cast<Ch *>(p + first_mmask16_index(masked)));
        }
        text = reinterpret_cast<Ch *>(p + i);
        return;
      }
      br = static_cast<std::uint16_t>(br & (br - 1u));
    }
    const std::uint16_t z =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_zero)));
    if (z)
      RAPIDXML_PARSE_ERROR("unexpected end of data",
                           reinterpret_cast<Ch *>(p + first_mmask16_index(z)));
    if (Flags & parse_validate_control_chars) {
      const std::uint16_t bad = simd_forbidden_control_mask(chunk);
      if (bad)
        RAPIDXML_PARSE_ERROR("unexpected control character",
                             reinterpret_cast<Ch *>(p + first_mmask16_index(bad)));
    }
    p += 16;
  }
}

template<typename Ch>
inline void simd_scan_until_gt_unrecognized(Ch *&text) {
  if (sizeof(Ch) != 1) {
    while (*text != Ch('>')) {
      if (*text == 0)
        RAPIDXML_PARSE_ERROR("unexpected end of data", text);
      ++text;
    }
    return;
  }
  unsigned char *p = reinterpret_cast<unsigned char *>(text);
  const simde__m128i v_zero = simde_mm_setzero_si128();
  const simde__m128i v_gt = simde_mm_set1_epi8(static_cast<char>('>'));
  while (true) {
    const simde__m128i chunk = simde_mm_loadu_si128(reinterpret_cast<const simde__m128i *>(p));
    const std::uint16_t gt =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_gt)));
    if (gt) {
      text = reinterpret_cast<Ch *>(p + first_mmask16_index(gt));
      return;
    }
    const std::uint16_t z =
        static_cast<std::uint16_t>(simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_zero)));
    if (z)
      RAPIDXML_PARSE_ERROR("unexpected end of data",
                           reinterpret_cast<Ch *>(p + first_mmask16_index(z)));
    p += 16;
  }
}

} // namespace rapidxml

#endif
