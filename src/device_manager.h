typedef struct DevicePort{
  int device_number;
  int port_number;
} DevicePort;

DevicePort *device_manager_get_DevicePort(gchar *staff_DP);
GtkWidget * DeviceManager (void);
GList *device_manager_DevicePort_list();


