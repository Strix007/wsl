return {
    "gelguy/wilder.nvim",
    dependencies = {
        { 'romgrk/fzy-lua-native' },
    },
    config = function()
        local wilder = require('wilder')
        wilder.setup({ modes = { ':', '/', '?' } })
        wilder.set_option('renderer', wilder.popupmenu_renderer(
            wilder.popupmenu_palette_theme({
                highlighter = {
                    wilder.lua_pcre2_highlighter(), -- requires `luarocks install pcre2`
                    wilder.lua_fzy_highlighter(),
                },
                highlights = {
                    accent = wilder.make_hl('WilderAccent', 'Pmenu', { { a = 1 }, { a = 1 }, { foreground = '#ebcb8b' } }),
                },
                border = 'rounded',
                max_height = '75%',
                min_height = 0,
                prompt_position = 'bottom',
                reverse = 1, -- if 1, shows the candidates from bottom to top
            }),
            {
                highlights = {
                accent = wilder.make_hl('WilderAccent', 'Pmenu', {{a = 1}, {a = 1}, {foreground = '#f4468f'}}),
                },
            }
        ))
    end,
}
