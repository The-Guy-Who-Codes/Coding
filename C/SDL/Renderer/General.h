#pragma once
#include <SDL2/SDL.h>



int init(SDL_Window **window, SDL_Renderer **renderer, SDL_Texture **texture, int SCREEN_WIDTH, int SCREEN_HEIGHT)
{
    //Initialization flag
    int success = 1;
    //Initialize SDL
    if( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
        printf( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError() );
        success = 0;
    }
    else
    {
        //Create window
        *window = SDL_CreateWindow( "Window", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN );
        if( window == NULL )
        {
            printf( "Window could not be created! SDL_Error: %s\n", SDL_GetError() );
            success = 0;
        }
        else
        {
            //Get window surface
            *renderer = SDL_CreateRenderer(*window, -1, SDL_RENDERER_SOFTWARE);
             if( renderer == NULL )
            {
                printf( "Renderer could not be created! SDL Error: %s\n", SDL_GetError() );
                success = 0;
            } else {
                *texture = SDL_CreateTexture(*renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, SCREEN_WIDTH, SCREEN_HEIGHT);
                if (texture == NULL) {
                    printf("Texture could not be created! SDL_Error:%s\n", SDL_GetError());
                    success = 0;
                }
            }
        }
    }

    return success;
}

void ex(SDL_Window *window, SDL_Renderer *renderer, SDL_Texture *texture)
{
    //Free loaded image
    SDL_DestroyTexture( texture );
    texture = NULL;

    //Destroy window    
    SDL_DestroyRenderer( renderer );
    SDL_DestroyWindow( window );
    window = NULL;
    renderer = NULL;

    //Quit SDL subsystems
    SDL_Quit();
}