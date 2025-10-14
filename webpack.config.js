const path = require('path');

module.exports = {
    entry: './src/index.ts',
    module: {
        rules: [
            {
                test: /\.js$/,
                // Disable handling of `new URL()` so directory URLs in dependencies don't break the build
                parser: { url: false },
            },
            {
                test: /\.ts?$/,
                use: {
                    loader: 'ts-loader',
                    options: {
                        transpileOnly: true,
                    },
                },
                exclude: /node_modules/,
            },
        ],
    },
    mode: 'production',
    resolve: {
        // Prefer built JS from node_modules to avoid loading TS sources with decorators
        extensions: ['.js', '.ts', '.tsx'],
        alias: {
            'leaflet-element$': require('path').resolve(__dirname, 'node_modules/leaflet-element/index.js'),
        },
    },
    output: {
        filename: 'bundle.js',
        path: path.resolve(__dirname, 'elm-pkg-js'),
    },
};
