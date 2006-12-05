package org.kalypso.afgui.viz;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.naming.ldap.Control;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.events.IExpansionListener;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.afgui.model.impl.TaskGroup;

public class WorkflowControl
{
	
	class SectionListener implements IExpansionListener
	{

		private Section lastExpanded;
		
		public void expansionStateChanged(ExpansionEvent e)
		{
			if(lastExpanded!=null)
			{
//				lastExpanded.setExpanded(false);
//				lastExpanded.redraw();
			}
			lastExpanded=(Section)e.getSource();
		}

		public void expansionStateChanging(ExpansionEvent e)
		{
			
		}		
		
		public Section getLastExpanded()
		{
			return lastExpanded;
		}
	};
	
	final static public String URI="_URI_"; 
	private IWorkflow workflow;
	private List<Section> stgSecs;
	private List<Section> taskGroupECs;
	private List<Section> phaseCntls;
	
	private Composite top;
	private boolean allowMultiPhase=false;
	private boolean allowMultiTG=false;
	private boolean allowMultiSTG=false;
	
	private FormToolkit toolkit;
	private ScrolledForm form;
	private Composite toolBarComp;
	private Composite taskComposite;
	
	ExpansionAdapter expansionAdapter=
		new ExpansionAdapter() {
			public void expansionStateChanged(ExpansionEvent e) {
				form.reflow(true);
			}
		};
	
	SectionListener tgEL = new SectionListener(); 
	SectionListener pEL = new SectionListener();
	
//		new IExpansionListener()
//	{
//
//		Section lastExpanded;
//		
//		public void expansionStateChanged(ExpansionEvent e)
//		{
//			if(lastExpanded!=null)
//			{
//				lastExpanded.setExpanded(false);
//				lastExpanded.redraw();
//			}
//			lastExpanded=(Section)e.getSource();
//		}
//
//		public void expansionStateChanging(ExpansionEvent e)
//		{
//			
//		}		
//	};
	
	public WorkflowControl(IWorkflow workflow)
	{
		this.workflow=workflow;
	}
	
	public void createControl(Composite parent)
	{
		createBaseContainers(parent);
		///IPhase
		phaseCntls=
			new ArrayList<Section>();
		Section madeSec;
		for(IPhase phase:workflow.getPhases())
		{
			madeSec=createPhaseExpandable(phase,form);
			phaseCntls.add(madeSec);
			madeSec.addExpansionListener(pEL);
			toolkit.createCompositeSeparator(madeSec);
		}
				
		taskGroupECs=
					new ArrayList<Section>();
		
		IPhase phase;
		for(Section ec:phaseCntls)
		{
			phase=(IPhase)ec.getData(URI);
			Composite comp=toolkit.createComposite(ec,SWT.BORDER);
			comp.setLayout(new GridLayout());
			for(ITaskGroup tg:phase.getTaskGroups())
			{
				madeSec=createTaskGroupExpandable(tg,comp);
				taskGroupECs.add(madeSec);
				madeSec.addExpansionListener(tgEL);
				toolkit.createCompositeSeparator(madeSec);
			}
			ec.setClient(comp);
		}
		
		
		///SubTaskGroup
		ITaskGroup taskGroup;
		stgSecs= new ArrayList<Section>(); 
		for(Section sec:taskGroupECs)//;int i=0;i<taskGroupECs.size();i++)
		{
			//System.out.println("III="+i);
			
			taskGroup=(ITaskGroup)sec.getData(URI);
			Composite comp=toolkit.createComposite(sec,SWT.BORDER);
			comp.setLayout(new GridLayout());
			for(ISubTaskGroup stg:taskGroup.getSubTaskGroups())
			{
				madeSec=createSubTaskGroupExpandable(stg,comp);
				stgSecs.add(madeSec);
			}
			sec.setClient(comp);
		}
		
////		/ITask
//		ISubTaskGroup subTaskGroup;
//		List<Button> taskButtons= new ArrayList<Button>(); 
//		for(Section sec:stgSecs)//;int i=0;i<taskGroupECs.size();i++)
//		{
//			//System.out.println("III="+i);
//			
//			subTaskGroup=(ISubTaskGroup)sec.getData(URI);
//			Composite comp=toolkit.createComposite(sec,SWT.BORDER);
//			
//			comp.setLayout(new GridLayout());
//			for(ITask t:subTaskGroup.getTasks())
//			{
//				Button taskButton=createTaskButton(t,comp);
//				taskButtons.add(taskButton);
//			}
//			sec.setClient(comp);
//		}
	}

	Map<Object, List<Control>> taskControlMap;
	
	private void contributeSTGTasks(IPhase phase)
	{
//		taskComposite.
//		toolkit.createTree(parent, style)
//		taskComposite.setLayout(new StackLayout());
//		
//		///ITask
//		ISubTaskGroup subTaskGroup;
//		List<Button> taskButtons= new ArrayList<Button>(); 
//		
//		for(Section sec:stgSecs)//;int i=0;i<taskGroupECs.size();i++)
//		{
//			//System.out.println("III="+i);
//			
//			subTaskGroup=(ISubTaskGroup)sec.getData(URI);
//			Composite comp=toolkit.createComposite(sec,SWT.BORDER);
//			
//			comp.setLayout(new GridLayout());
//			for(ITask t:subTaskGroup.getTasks())
//			{
//				Button taskButton=createTaskButton(t,comp);
//				taskButtons.add(taskButton);
//			}
//			sec.setClient(comp);
//		}

	}
	
	private Button createTaskButton(ITask t, Composite comp)
	{
		String name=getWorkflowPartName(t);
		Button b= toolkit.createButton(comp, name, SWT.NONE);
		return b;
	}

	private Section createSubTaskGroupExpandable(
			ISubTaskGroup stg, Composite ec)
	{
		Section childEC = 
		toolkit.createSection(
			ec, 
			Section.TREE_NODE|
			Section.CLIENT_INDENT|
			Section.TWISTIE);
		String name=getWorkflowPartName(stg);
		childEC.setText(name);
		childEC.setData(URI, stg);
		
		return childEC;
		
	}

	
	private void createBaseContainers(Composite parent)
	{
		top = new Composite(parent,SWT.FILL);
		top.setLayout(new FillLayout());
		toolkit= new FormToolkit(top.getDisplay());
		Composite containerForm=toolkit.createComposite(top);
		
			
		containerForm.setLayout(new FormLayout());
		
		FormData fd;
		
		fd= new FormData();
		fd.left= new FormAttachment(0,0);
		fd.bottom= new FormAttachment(70,0);
		fd.top= new FormAttachment(0,0);
		form = 
			toolkit.createScrolledForm(containerForm);
		form.setLayoutData(fd);
		
		fd= new FormData();
		fd.width=16;
		fd.left=new FormAttachment(form);
		fd.right= new FormAttachment(90,0);
		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(0,0);
		
		toolBarComp= 
			toolkit.createComposite(containerForm, SWT.BORDER|SWT.BOLD);
		toolBarComp.setLayout(new FillLayout());
		toolBarComp.setLayoutData(fd);
		toolkit.createButton(
				toolkit.createComposite(toolBarComp, SWT.BORDER),
				"DADA",
				SWT.BUTTON1);

		taskComposite=
			toolkit.createComposite(
					containerForm,
					SWT.BORDER|SWT.BOLD);
		
		fd= new FormData();
		fd.left= new FormAttachment(0,0);
		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(30,0);
		fd.right=new FormAttachment(toolBarComp);
		taskComposite.setLayoutData(fd);
		taskComposite.setLayout(new FillLayout());
		form.getBody().setLayout(new TableWrapLayout());
	}
	
	private Section createTaskGroupExpandable(
								ITaskGroup tg, Composite ec)
	{
		Section childEC = 
			toolkit.createSection(
						ec, 
						Section.TREE_NODE|
						Section.CLIENT_INDENT|
						Section.TWISTIE);
		
		childEC.setText(
				getWorkflowPartName(tg));
		childEC.setData(URI, tg);
		
		//childEC.setLayout(new TableWrapLayout());
		
		return childEC;
		
	}

	private Section createPhaseExpandable(
									IPhase phase, 
									ScrolledForm form)
	{
		Section ec = 
			toolkit.createSection(
						form.getBody(), 
						Section.TREE_NODE|
						Section.CLIENT_INDENT|
						Section.TWISTIE);
		ec.setText(
				getWorkflowPartName(phase));
		ec.setData(URI, phase);
		//ec.setLayout(new TableWrapLayout());
		return ec;
	}
	
	final static public String getWorkflowPartName(IWorkflowPart wp)
	{
		String name=wp.getName();
		if(name==null)
		{
			name=wp.getURI();
		}
		if(name.equals(""))
		{
			name=wp.getURI();
		}
		return name;
	}
	
}
