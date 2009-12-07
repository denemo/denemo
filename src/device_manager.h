typedef struct DevicePort{
  int device_number;
  int port_number;
} DevicePort;

DevicePort device_manager_get_DevicePort(gchar *staff_DP);
GtkWidget *DeviceManager ();
void device_manager_refresh_model(void);
void device_manager_create_device();
void device_manager_remove_device();
void device_manager_create_port();
void device_manager_remove_port();
GList *device_manager_DevicePort_list();


