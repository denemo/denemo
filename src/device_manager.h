typedef struct DevicePort{
  int device_number;
  int port_number;
} DevicePort;

DevicePort *device_manager_get_DevicePort(gchar *staff_DP);
void DeviceManager (GtkWidget *main_vbox);
GList *device_manager_DevicePort_list();


