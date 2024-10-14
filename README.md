# IP Camera

This application allows you to view live video stream from any IP camera supporting RTSP protocol.
Additionally, the application allows you to configure pan, tilt and zoom actions for the PTZ cameras.

# Installation

To be added...

# Usage

Adding a new camera requires manually configuring the RTSP URI and pan, tilt and zoom action requests.

The software does not support the ONVIF standard at the present time.

## Adding a new camera

After opening the application, the first thing to do is to add a new camera.

### Basic configuration

1. Click the `+` button in the top left corner.
   A camera configuration window will appear.
2. Enter a unique name for the camera
3. Enter an URI to the RTSP stream.
   The URI must be in format: `rtsp://[username:password@]{host}/{path}`
   > [!NOTE]
   > The URI path will be different for different camera models, but leaving it empty is a good first guess.
   > If empty URI path doesn't work, you will have to do a bit of research of your particular model.
   > Inspecting the Javascript of the web interface with the web browser developer tools can be used for that purpose.
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
