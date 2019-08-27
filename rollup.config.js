import { DEFAULT_EXTENSIONS } from '@babel/core';
import commonjs from 'rollup-plugin-commonjs';
import nodeResolve from 'rollup-plugin-node-resolve';
import buble from 'rollup-plugin-buble';
import babel from 'rollup-plugin-babel';
import { terser } from 'rollup-plugin-terser';

const plugins = [
  nodeResolve({
    mainFields: ['module', 'jsnext', 'main'],
    browser: true
  }),
  commonjs({
    include: /\/node_modules\//
  }),
  buble({
    transforms: {
      dangerousForOf: true,
      dangerousTaggedTemplateString: true
    },
    objectAssign: 'Object.assign'
  }),
  babel({
    babelrc: false,
    extensions: [...DEFAULT_EXTENSIONS, 'ts', 'tsx'],
    exclude: 'node_modules/**',
    presets: [],
    plugins: [['babel-plugin-closure-elimination', {}]]
  }),

  terser({
    warnings: true,
    ecma: 5,
    ie8: false,
    mangle: true,
    toplevel: true,
    output: {
      beautify: true,
      braces: true,
      indent_level: 2
    }
  })
];

const config = {
  input: './src/index.js',
  external: () => false,
  plugins,
  treeshake: {
    propertyReadSideEffects: false
  },
  output: [
    {
      legacy: true,
      freeze: false,
      esModule: false,
      file: './dist/pessimism.js',
      format: 'cjs'
    },
    {
      legacy: true,
      freeze: false,
      esModule: false,
      file: './dist/pessimism.es.js',
      format: 'esm'
    }
  ]
};

export default config;
