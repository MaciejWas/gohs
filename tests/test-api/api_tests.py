import asyncio
from typing import Any

import redis
import requests
from requests import Response

def assert_eq(a: Any, b: Any):
    if a != b:
        raise Exception(f"{a} != {b}")


# {"contents":[0,["contents":16781741,"tag":"V4"},"olcia","ola"]],"tag":"LoginRequest"}

async def auth_can_register():
    r = redis.Redis("redis", 6379)
    request_id = 0
    username = "ala"
    password = "kot"
    message = {"contents": [request_id, [username, password]], "tag":"RegisterRequest"}
    r.xadd("auth:requests", fields={"data": message})
    result = await r.xread({"auth:requests": "$"}, count=1, block=True)

tests = {
    "app_responds_to_ping": auth_can_register,
}

if __name__ == '__main__':

    print(("=" * 20) + "  RUNNING TESTS  " + ("=" * 20))

    for test_name in tests:
        print(" . " + test_name, end="\r")
        asyncio.run(tests[test_name]())
        print(" ✓ ")

    print(("=" * 20) + "  TESTS PASSED  " + ("=" * 20))
