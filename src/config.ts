interface Config {
    /** Navigate to https://developer.riotgames.com/ to get the token. It expires every 24 hours */
    apiToken: string;

    /** How to find gameName and tagLine:
        By hovering over your profile icon in the League of Legends client app
        in the upper right corner of the UI window. A hovering panel will appear with text, such as
        [ MrPlayer #EUW ]
        MrPlayer is your gameName.
        #EUW is your tagLine. It should be specified without the '#' character. */
    gameName: string;
    tagLine: string;
}