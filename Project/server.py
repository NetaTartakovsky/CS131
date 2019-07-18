import asyncio
import aiohttp
import time
import sys
import re

servers = ['Goloman', 'Hands', 'Holiday', 'Welsh', 'Wilkes']
port_keys = {'Goloman': 11505, 'Hands': 11506, 'Holiday': 11507, 'Welsh': 11508, 'Wilkes': 11509}
connections = {'Goloman': ['Hands', 'Holiday', 'Wilkes'], 'Hands': ['Goloman', 'Wilkes'], 'Holiday': ['Goloman', 'Welsh', 'Wilkes'], 'Welsh': ['Holiday'], 'Wilkes': ['Goloman', 'Hands', 'Holiday']}
server_name = ""
server_logs = {'Goloman': {}, 'Hands': {}, 'Holiday': {}, 'Welsh': {}, 'Wilkes': {}}
log = ""

time_match = re.compile(r'^[0-9]*.[0-9]+$|^[0-9]+.[0-9]*$')
coord_match = re.compile(r'^[+|-][0-9]+.[0-9]+[+|-][0-9]+.[0-9]+$')

async def main():
    if len(sys.argv) != 2:
        print("Incorrect number of arguments!")
        sys.exit(1)
    if sys.argv[1] not in servers:
        print("Must input a valid server!")
        sys.exit(1)

    global server_name
    global log
    server_name = sys.argv[1]
    file_name = "./server_logs/" + server_name.lower() + ".log"
    log = open(file_name, 'w')
    server = await asyncio.start_server(handle_connection, host='127.0.0.1', port=port_keys[server_name])

    async with server:
        await server.serve_forever()

async def handle_connection(reader, writer):
    data = await reader.readline()
    time_received = time.time()
    log.write("Connected to %s \n" % server_name)
    message = data.decode()
    log.write("%s received: %s \n" % (server_name, message))
    args = message.split()
    error_msg = "? " + message
    if args[0] == 'IAMAT':
        if len(args) != 4:
            writer.write(error_msg.encode())
            log.write("Error: incorrect number of arguments \n")
        elif (not bool(coord_match.match(args[2]))):
            writer.write(error_msg.encode())
            log.write("Error: incorrect location format \n")
        elif (not bool(time_match.match(args[3]))):
            writer.write(error_msg.encode())
            log.write("Error: incorrect time format \n")
        else:
            time_diff = time_received - float(args[3])
            time_elapsed = str(time_diff)
            if time_diff > 0:
                time_elapsed = "+" + str(time_diff)
            at_msg = "AT " + server_name + " " + time_elapsed + " " + args[1] + " " + args[2] + " " + args[3]
            writer.write(at_msg.encode())
            log.write("Response to client: %s \n" % at_msg)
            if args[1] not in (server_logs[server_name]):
                (server_logs[server_name])[args[1]] = at_msg
            else:
                msg_args = ((server_logs[server_name])[args[1]]).split()
                print(msg_args)
                if float(msg_args[5]) <= float(args[3]):
                    (server_logs[server_name])[args[1]] = at_msg
            await flood(server_name, at_msg)
    elif args[0] == 'AT':
        if len(args) != 6:
            writer.write(error_msg.encode())
            log.write("Error: incorrect number of arguments \n")
        else:
            if args[3] not in server_logs[server_name]:
                (server_logs[server_name])[args[3]] = message
                await flood(server_name, message)
            else:
                if message != (server_logs[server_name])[args[3]]:
                    msg_args = ((server_logs[server_name])[args[3]]).split()
                    if float(msg_args[5]) <= float(args[5]):
                        (server_logs[server_name])[args[3]] = message
                        await flood(server_name, message)
    elif args[0] == 'WHATSAT':
        if len(args) != 4:
            writer.write(error_msg.encode())
            log.write("Error: incorrect number of arguments \n")
        elif (not args[2].isdigit()) or (not args[3].isdigit()):
            writer.write(error_msg.encode())
            log.write("Error: radius and bound must be integers \n")
        elif (int(args[2]) > 50) or (int(args[2]) < 0):
            writer.write(error_msg.encode())
            log.write("Error: radius out of bounds \n")
        elif (int(args[3]) > 20) or (int(args[3]) < 0):
            writer.write(error_msg.encode())
            log.write("Error: bound out of bounds \n")
        else:
            if args[1] not in server_logs[server_name]:
                writer.write(error_msg.encode())
                log.write("Error: client not in records \n")
            else:
                at_msg = (server_logs[server_name])[args[1]]
                at_args = at_msg.split()
                writer.write(at_msg.encode())
                info = await request(at_args[4], args[2], args[3])
                writer.write(info.encode())
                log.write("Response to client: %s %s \n" % (at_msg, info))

    else:
        writer.write(error_msg.encode())
        log.write("Error: command must be IAMAT, AT, or WHATSAT \n")

    writer.close()
    log.write("%s closed the connection \n" % server_name)

async def flood(server, message):
    for friend in connections[server]:
        reader, writer = await asyncio.open_connection(host='127.0.0.1', port=port_keys[friend])
        writer.write(message.encode())
        writer.close()

async def fetch(session, url):
    async with session.get(url) as response:
        return await response.text()

async def request(coords, radius, bound):
    # turn coords into proper format for http request
    latitude = ""
    longitude = ""
    if coords[0] == '-':
        latitude += '-'
    i = 1
    while coords[i].isdigit() or coords[i] == '.':
        latitude += coords[i]
        i += 1
    if coords[i] == '-':
        longitude += '-'
    i += 1
    while i < len(coords):
        longitude += coords[i]
        i += 1
    location = "location=" + latitude + "," + longitude
    radius = "&radius=" + radius
    key = "&key=" + "AIzaSyBg65M32DjTAMgibhRpXJesoLUUWrag9FM"
    url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?" + location + radius + key

    async with aiohttp.ClientSession() as session:
        result = '\n'
        dump = await fetch(session, url)
        parsed = dump.split('\n')
        i = 0
        j = 0
        while (i < len(parsed)-1):
            if parsed[i] == '      },' and parsed[i+1] == '      {':
                j += 1
            result += parsed[i] + '\n'
            if j == int(bound):
                break
            i += 1
        if j < int(bound):
            result += parsed[i] + '\n'
        return result


if __name__ == '__main__':
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        pass
