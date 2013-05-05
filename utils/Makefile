all:generate_commands

generate_commands:
	gcc generate_commands.c -o generate_commands `pkg-config --cflags --libs libxml-2.0 glib-2.0` -Wall

clean:
	rm -rf generate_commands
