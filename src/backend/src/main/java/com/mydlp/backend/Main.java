package com.mydlp.backend;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

import sun.management.VMManagement;

public class Main {

	public static void main(String[] args) {
		String appDir = System.getProperty("mydlp.appdir");
		if (appDir != null)
		{
			createPidFile(appDir);
		}
		BackendServer server = new BackendServer();
		server.listen();
	}
	
	protected static void createPidFile(String appDir) {
		File f = new File(appDir);
		f = new File(f, "run");
		f = new File(f, "backend.pid");
		try {
			BufferedWriter bw = new BufferedWriter(new FileWriter(f));
			bw.write(getPid().toString());
			bw.flush();
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	protected static Integer getPid() {
		try {
			RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
			Field jvmField = runtimeMXBean.getClass().getDeclaredField("jvm");
			jvmField.setAccessible(true);
			VMManagement vmManagement = (VMManagement) jvmField.get(runtimeMXBean);
			Method getProcessIdMethod = vmManagement.getClass().getDeclaredMethod("getProcessId");
			getProcessIdMethod.setAccessible(true);
			Integer processId = (Integer) getProcessIdMethod.invoke(vmManagement);
			return processId;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return 0;
	}

}
