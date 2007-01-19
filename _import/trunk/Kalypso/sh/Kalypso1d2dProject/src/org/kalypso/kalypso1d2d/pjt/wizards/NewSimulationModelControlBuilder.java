package org.kalypso.kalypso1d2d.pjt.wizards;




import java.util.List;

import javax.swing.colorchooser.ColorChooserComponentFactory;
import javax.swing.text.StyleConstants.ColorConstants;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.afgui.db.EWorkflowProperty;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;



/**
 * @author Patrice Congo
 *
 */
public class NewSimulationModelControlBuilder
{
	
	final static String NEW_NAME_MUST_NOT_BE_EMPTY="Namem feld kann nicht leer sein";
	final static String ALLREADY_EXISTS="Simulation mit diesem namen existiert schon";
	final static private Logger logger=
			Logger.getLogger(NewSimulationModelControlBuilder.class);
	
	private String errorMessage; 
	
	private IWorkflowData parentWorkflowData;
	
	private Composite panel;

	private Text parentTFE;
	
	private Text newModelTFE;
	
	private Text commentText;
	
	private IUpdateListener updateListener;
	
	private String newName;

	private KeyListener keyListener= new KeyListener()
	{

		public void keyPressed(KeyEvent e)
		{
			//Empty
		}

		public void keyReleased(KeyEvent e)
		{
			cacheNewName();
			if(updateListener!=null)
			{
				updateListener.update();
			}
		}
		
	};
	
	public NewSimulationModelControlBuilder(
							IWorkflowData parentWorkflowData,
							Composite parentComposite)
	{
		this.parentWorkflowData=parentWorkflowData;
		createControl(parentComposite);
		cacheNewName();
		updateContent();
	}
	
	private  void createControl(Composite parent)
	{
		panel= new Composite(parent, SWT.FILL);
		GridLayout gl = new GridLayout();
		gl.numColumns=2;
		panel.setLayout(gl);
		panel.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		Label newModelNameLabel= new Label(panel,SWT.NONE);
		newModelNameLabel.setText("Name:");
		newModelTFE= 
			new Text(panel,SWT.BORDER);
		newModelTFE.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		newModelTFE.addKeyListener(keyListener);
		
		Label parentLabel= new Label(panel,SWT.NONE);
		parentLabel.setText("Parent:");
		parentTFE= new Text(panel,SWT.BORDER);
		parentTFE.setEditable(false);
		parentTFE.setText(getParentDataName());
		parentTFE.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		Label commentLabel=new Label(panel, SWT.NONE);
		commentLabel.setText("Comment:");
		commentText= new Text(panel, SWT.BORDER|SWT.WRAP|SWT.MULTI);
		GridData gd= new GridData(GridData.FILL_BOTH);
		gd.verticalSpan=10;
		commentText.setLayoutData(gd);
		
		//VerifyListener vl;
			
	}
	private String getParentDataName()
	{
		if(parentWorkflowData==null)
			
		{
			return "";
		}
		String pname=parentWorkflowData.getName();
		if(pname==null)
		{
			pname="";
		}
		return pname;
	}
	
	private void updateContent()
	{
		if(parentWorkflowData==null)
		{
			parentTFE.setText("");
		}
		else
		{
			parentTFE.setText(parentWorkflowData.getName());
		}
	}
	
	public IWorkflowData getParent()
	{
		return parentWorkflowData;
	}
	
	public String cacheNewName()
	{
		newName= newModelTFE.getText();
		
		newName=(newName==null)?"":newName.trim();
		return newName;
	}
	
	public String getNewName()
	{
		return newName;
	}
	
	public String getComment()
	{
		//FormLayout fl=null;
		
		String comment=commentText.getText();
		return comment;
	}
	
	
	public boolean isValid()
	{
		Boolean answer=true;
		StringBuffer errors= new StringBuffer(128);
		if(newName.equals(""))
		{
			answer=answer&&false;		
			errors.append(NEW_NAME_MUST_NOT_BE_EMPTY);
		}
		
		errorMessage=errors.toString();
		return answer;
	}
	
	
	public Control getControl()
	{
		return panel;
	}
	
	public String getErrorMessage()
	{
		return errorMessage;
	}
	
	public void setUpdateListerner(IUpdateListener updateListener)
	{
		this.updateListener=updateListener;
	}
	
	
	public static void main(String args[])
	{
		IWorkflowData parentData=
			new IWorkflowData()
		{

			public List<IWorkflowData> getLinkedWorkflowData(EWorkflowProperty prop)
			{
				return null;
			}

			public String getLocation()
			{
				return null;
			}

			public Object getModelObject()
			{
				return null;
			}

			public String getType()
			{
				return Kalypso1D2DSchemaConstants.SIMULATION_MODEL1D2D.toString();
			}

			public String getURI()
			{
				return "www.dada.de/dde";
			}

			public boolean hasLinkedWorkflowData(EWorkflowProperty prop)
			{
				return false;
			}
			
			public String getName()
			{
				return "Sim1";
			}
			
			public void remove()
			{
				
			}
			
		};
		Display d = new Display();
		final Shell shell = new Shell(d);
		shell.setSize(400, 400);
		shell.setText("Wizard Test");
		shell.setLayout(new FillLayout());
		Composite c= new Composite(shell,SWT.FILL);
		c.setLayout(new GridLayout());
		c.setLayoutData(new GridData(SWT.FILL));
		NewSimulationModelControlBuilder page= 
				new NewSimulationModelControlBuilder(parentData,c);
		//page.createControl(c);
//		new Label(c,SWT.NULL).setText("DDDDDDDDDDDDD");
		//shell.pack();
		shell.open();
		page.isValid();
		
		startWizard(shell,parentData);
		
		
		while (!shell.isDisposed())
		{
			while (!d.readAndDispatch())
			{
				d.sleep();
			}
		}
	 }

	///////////
	final static public void startWizard(Shell shell, IWorkflowData workflowData )
	{
		logger.info("starting wizard");
		NewSimulationModelWizardPage wpage=
			new NewSimulationModelWizardPage(
							"Neues Simulation Model",
							workflowData);
		
		Wizard iWizard=new Wizard()
		{
			
			@Override
			public boolean performFinish()
			{
				NewSimulationModelWizardPage page=
					(NewSimulationModelWizardPage)getStartingPage();
				logger.info("page:"+page.getClass());
				boolean answer=
					page.getNewSimulaionControlBuilder().isValid();
				if(!answer)
				{
					page.setErrorMessage("");
				}
				return answer;
			}
			
		};
		
		iWizard.addPage(wpage);
		//wpage.setTitle("spage");
		WizardDialog wd= new WizardDialog(shell,iWizard);
		wd.setTitle("Neue Simulationsmodel");
		//wd.setMessage("Neue Simulationsmodell");
		//wd.setBlockOnOpen(true);
		int decision=wd.open();
		logger.info("Opendecision:"+decision);
		if(decision==WizardDialog.OK)
		{
			String name=wpage.getNewSimulaionControlBuilder().getNewName();
			logger.info("newName="+name);
			IWorkflowDB workflowDB=
				ActiveWorkContext.getInstance().getWorkflowDB();
			if(workflowDB==null)
			{
				logger.warn("no workflow db available");
				return;
			}
			else
			{
				workflowDB.createWorkflowData(
						name, 
						Kalypso1D2DSchemaConstants.SIMULATION_MODEL1D2D.toString(), 
						workflowData);
			}
		}
		else
		{
			logger.info("Wizard canceled:"+decision);
		}
	};
}
