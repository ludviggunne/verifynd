/**
 * @file Tree sitter grammar for the verifynd proof format
 * @author Ludvig Lindström <ludviggunnelindstrom@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "verifynd",

  extras: $ => [
    /\s/,
    $.comment,
  ],

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => $.proof,

    proof: $ => repeat1($.entry),

    entry: $ => choice(
      $.line,
      $.box
    ),

    box: $ => seq(
      $.lbrace,
      $.proof,
      $.rbrace
    ),

    lbrace: _ => token('{'),

    rbrace: _ => token('}'),

    line: $ => seq(
      $.number,
      $.formula,
      $.rule,
      optional($.reference_list),
      $.semi,
    ),

    number: $ => token(
      /[1-9][0-9]*/
    ),

    semi: _ => token(';'),

    formula: $ => choice(
      $.atom,
      $.binop,
    ),

    binop: $ => seq(
      $.atom,
      choice(
        $.impl,
        $.and,
        $.or
      ),
      $.atom
    ),

    impl: _ => token('->'),
    and: _ => token('&&'),
    or: _ => token('||'),

    atom: $ => choice(
      $.parenthesized,
      $.not_expr,
      $.contradiction,
      $.variable
    ),

    parenthesized: $ => seq(
      $.lparen,
      $.formula,
      $.rparen,
    ),

    lparen: _ => token('('),

    rparen: _ => token(')'),

    not_expr: $ => seq(
      $.not,
      $.atom
    ),

    not: _ => token('!'),

    contradiction: $ => token('?'),

    variable: _ => token(
      /[a-zA-Z]+/
    ),

    reference_list: $ => seq(
      $.reference,
      repeat(seq(
        $.comma,
        $.reference,
      ))
    ),

    comma: _ => token(','),

    reference: $ => choice(
      $.line_ref,
      $.box_ref,
    ),

    line_ref: $ => $.number,

    box_ref: $ => seq(
      $.number,
      $.hyphen,
      $.number
    ),

    hyphen: _ => token('-'),

    rule: $ => token(choice(
      'premise',
      'assumption',
      'LEM',
      'PBC',
      'MT',
      'copy',
      '->e',
      '->i',
      '&&i',
      '&&e1',
      '&&e2',
      '||i1',
      '||i2',
      '||e',
      '!i',
      '!e',
      '!!e',
      '?e'
    )),

    comment: _ => token(seq(
      '//',
      /[^\n]*/
    ))
  }
});
