# IP Camera

This application allows you to view live video stream from any IP camera supporting RTSP protocol.
Additionally, the application allows you to configure pan, tilt and zoom actions for the PTZ cameras.

# Installation

To be added...

# Usage

Adding a new camera requires manually configuring the RTSP URI and pan, tilt and zoom action requests.

Proper configuration requires some basic knowledge about the URIs.

A URI can be decomposed into the following components:

```
rtsp:// [username:password@] host [:port] / path
```

Where
* `username` and `password` are your login credentials to your IP camera
* `host` is the IP address or host name of your IP camera
* `port` is the network port. Skipping this field is a good first guess.
* `path` is the path to a particular resource, such as the video stream, on the device.

> [!NOTE]
> The URI paths and PTZ action requests will be different on different camera models.
> In case of URI to the RTSP stream, a URI with an empty `path` is a good first guess.
> If such URI path doesn't work, you will have to do a bit of research on your particular model.
> Inspecting the Javascript of the web interface with the web browser developer tools can be used for that purpose.

The software does not support the ONVIF standard at the present time.

## Adding a new camera

After opening the application, the first thing to do is to add a new camera.

### Basic configuration

1. Click the `+` button in the top left corner.
   A camera configuration window will appear.
2. Enter a unique name for the camera
3. Enter an URI to the RTSP stream.
   The URI must be in format: `rtsp:// [username:password@] host [:port] / path`, for example `rtsp://admin:pass123@192.168.1.1/0/stream`.
4. Click the save button.

The new camera will appear in the list on the left.

### Pan-tilt-zoom configuration

TODO

### Editing existing camera

TODO

## Connecting to the camera

After a camera was added, you can connect to it by selecting it from the list on the left.
Click any camera from the list to connect immediately and view the video stream.

### Pan-tilt-zoom actions

If the pan-tilt-zoom actions were configured, you can use the buttons in the bottom right corner.
