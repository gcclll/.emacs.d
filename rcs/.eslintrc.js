module.exports = {
  parser: 'babel-eslint',
  parserOptions: {
    sourceType: 'module',
    ecmaVersion: 6,
    ecmaFeatures: {
      // 不允许全局使用 return
      globalReturn: false,
      // 全局启用严格模式
      impliedStrict: true,
      // 启用 es7的 spread operator, etc.
      experimentalObjectRestSpread: true
    }
  },
  plugins: ['html'],
  env: {
    browser: true,
    es6: true,
    node: true
  },
  extends: 'eslint:recommended',
  rules: {
    '@typescript-eslint/no-unnecessary-condition': ['error'],
    semi: ['error', 'never'],
    // 由于默认禁用了，在这开启下
    'no-console': 'off',
    // 禁止在else前有return，或者if里面有return了，后面就不能有else了
    // 其实 if () { return true; } else { return false; } 后面的else
    // 就是多余的
    'no-else-return': 'error',
    // 使null 只和null比较，一般情况下null == undefined，为了避免这种情况
    // 出现，减少bug可能，只让和null以及用恒等去比较
    'no-eq-null': 'error',
    // 禁用eval
    'no-eval': 'error',
    // 禁止循环中声明函数，除非声明完立即执行
    // "no-loop-func": "error",
    // 禁止对原始包装对象使用new, (String、Number 和 Boolean)
    'no-new-wrappers': 'error',
    // 强制使用parseInt时候指定进制数，as-needed进制使用10
    radix: 'error',
    // 强制async函数里必须要有await
    'require-await': 'error',
    // 禁止拖尾逗号
    'comma-dangle': ['error', 'never'],
    // 强制文件末尾有一空行
    'eol-last': ['error', 'always'],
    // 强制使用let, const
    'no-var': 'error',
    // 两个空格缩进
    indent: ['error', 2],
    // js 必须使用单引号或者模板字符串
    quotes: ['error', 'single', { allowTemplateLiterals: true }],
    // 逗号后面使用空格
    'comma-spacing': 'error',
    // 禁止小括号之前的空格
    'space-before-function-paren': ['error', 'never']
  }
}
