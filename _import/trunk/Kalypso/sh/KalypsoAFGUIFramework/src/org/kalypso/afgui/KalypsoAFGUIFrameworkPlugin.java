package org.kalypso.afgui;

import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;

import org.apache.log4j.Logger;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.model.EActivityAction;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.afgui.model.impl.WorkflowSystem;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoAFGUIFrameworkPlugin extends AbstractUIPlugin {

	public static final String TEMPLATE_WORKFLOW_DATA=
								"/workflow/template_workflow_data.xml";
	public static final String WORKFLOW_SPEC="/workflow/spec.xml";
	public static final String WORKFLOW_STATUS="/workflow/status.xml";
	
	
	private static final Logger logger=
			Logger.getLogger(KalypsoAFGUIFrameworkPlugin.class);
	
	// The plug-in ID
	public static final String PLUGIN_ID = "org.kalypso.afgui.KalypsoAFGUIFramework";
	
	
	// The shared instance
	private static KalypsoAFGUIFrameworkPlugin plugin;
	
	//private IWorkflow workflow;
	private IWorkflowSystem workflowSystem;
	/**
	 * The constructor
	 */
	public KalypsoAFGUIFrameworkPlugin() {
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
//		//this.workflow=createWorkflow(context.getBundle());
//		this.workflowSystem=createWorkflowSystem(context.getBundle());
//		logger.info("\n======Pluging Workflow"+workflowSystem);
	}

	final private static IWorkflowSystem createWorkflowSystem(Bundle bundle)
	{
		//Bundle bundle=context.getBundle();
		URL specURL= bundle.getEntry(WORKFLOW_SPEC);
		URL statusURL=bundle.getEntry(WORKFLOW_STATUS);
		
		try
		{
			WorkflowSystem system= new WorkflowSystem(specURL,statusURL);
			return system;
		}
		catch (IOException e)
		{
			e.printStackTrace();
			return null;
		}
	}
	
//	/**
//	 * This is a temporary test method to create a workflow from its specification
//	 * and static files without using projects
//	 * Only for test purpose. the actual should be based a project nature.
//	 * 
//	 * @return the test Workflow
//	 */
//	final private static IWorkflow createWorkflow(Bundle bundle)
//	{
//		//Bundle bundle=context.getBundle();
//		URL specURL= bundle.getEntry(WORKFLOW_SPEC);
//		URL statusURL=bundle.getEntry(WORKFLOW_STATUS);
//		WorkflowSystem builder= new WorkflowSystem();
//		try
//		{
//			return builder.createWorkflow(specURL, statusURL);
//		}
//		catch (IOException e)
//		{
//			e.printStackTrace();
//			return null;
//		}
//	}
	
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static KalypsoAFGUIFrameworkPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}
	
	public IWorkflowSystem getWorkflowSystem()
	{
		return this.workflowSystem;
	}
	
//	public static final String IMG_PATH_BASE_SWEEZIES="/icons/sweetie2/png/";
//	public static final String IMG_PATH_DO=IMG_PATH_BASE_SWEEZIES+"24-tools.png";
//	public static final String IMG_PATH_DOWN=IMG_PATH_BASE_SWEEZIES+"24-em-down.png";
//	public static final String IMG_PATH_GET_HELP=IMG_PATH_BASE_SWEEZIES+"24-message-info.png";
//	public static final String IMG_PATH_NEXT=IMG_PATH_BASE_SWEEZIES+"24-arrow-next.png";
//	public static final String IMG_PATH_PREVIOUS=IMG_PATH_BASE_SWEEZIES+"24-arrow-previous.png";
//	public static final String IMG_PATH_SHOW_DATA=IMG_PATH_BASE_SWEEZIES+"24-tab.png";
//	public static final String IMG_PATH_UP=IMG_PATH_BASE_SWEEZIES+"24-em-up.png";
//	//public static final String IMG_PATH_TOP=IMG_PATH_BASE_SWEEZIES+"top.png";
	
	public static final String IMG_PATH_BASE_NUVOLA="/icons/nuvola_select/";
	public static final String IMG_PATH_DO=IMG_PATH_BASE_NUVOLA+"kig.png";
	public static final String IMG_PATH_DOWN=IMG_PATH_BASE_NUVOLA+"down.png";
	public static final String IMG_PATH_GET_HELP=IMG_PATH_BASE_NUVOLA+"quiz.png";
	public static final String IMG_PATH_NEXT=IMG_PATH_BASE_NUVOLA+"forward.png";
	public static final String IMG_PATH_PREVIOUS=IMG_PATH_BASE_NUVOLA+"back.png";
	public static final String IMG_PATH_SHOW_DATA=IMG_PATH_BASE_NUVOLA+"kalzium.png";
	public static final String IMG_PATH_UP=IMG_PATH_BASE_NUVOLA+"up.png";
	public static final String IMG_PATH_TOP=IMG_PATH_BASE_NUVOLA+"top.png";
	public static final String IMG_PATH_RELOAD=IMG_PATH_BASE_NUVOLA+"kaboodleloop.png";
	@Override
	protected void initializeImageRegistry(ImageRegistry reg)
	{
		super.initializeImageRegistry(reg);
		String couples[][]={
				{EActivityAction.DO.toString(),IMG_PATH_DO},
				{EActivityAction.DOWN.toString(),IMG_PATH_DOWN},
				{EActivityAction.GET_HELP.toString(),IMG_PATH_GET_HELP},
				{EActivityAction.NEXT.toString(),IMG_PATH_NEXT},
				{EActivityAction.PREVIOUS.toString(),IMG_PATH_PREVIOUS},
				{EActivityAction.SHOW_DATA.toString(),IMG_PATH_SHOW_DATA},
				{EActivityAction.UP.toString(),IMG_PATH_UP},
				{EActivityAction.TOP.toString(),IMG_PATH_TOP},
				{EActivityAction.RELOAD.toString(),IMG_PATH_RELOAD}
		};
				
		String curCouple[];
		for(int i=0; i<couples.length;i++)
		{
			curCouple=couples[i];
			URL url = getBundle().getEntry(curCouple[1]);//Platform.find(bundle, path);
			ImageDescriptor desc = ImageDescriptor.createFromURL(url);
			reg.put(curCouple[0], desc);
		}
		
	}
	
	public void reloadWorkflow()
	{
		try{
			//TODO adopt also for workflow System
			//this.workflow.adopt(createWorkflow(getBundle()));
		}catch(Throwable th)
		{
			showMessage(th.getMessage());
		}
		
	}
	
	public void showMessage(final String message)
	{

		plugin.getWorkbench().getDisplay().syncExec(
		        new Runnable() {
		           public void run(){
		        	   Shell shell=plugin.getWorkbench().getDisplay().getActiveShell();
		       		MessageDialog.openInformation(shell,"Message",message);
		           }
		        }
		     );
	}
	
	public URL getTemplateWorkflowData()
	{
		return getBundle().getResource(TEMPLATE_WORKFLOW_DATA);
	}
	
	public URL getWorkflowSpec()
	{
		return getBundle().getResource(WORKFLOW_SPEC);
	}
	
}
