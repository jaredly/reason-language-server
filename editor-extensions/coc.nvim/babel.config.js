const config = {
  presets: [
    [
      '@babel/env',
      {
        targets: {
          node: '10',
        },
      },
    ],
    '@babel/flow',
  ],
};

module.exports = config;
