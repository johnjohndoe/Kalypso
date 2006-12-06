package org.kalypso.afgui.viz;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;



import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
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
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ToolBar;
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
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.afgui.model.impl.TaskGroup;

import EDU.oswego.cs.dl.util.concurrent.Takable;

public class WorkflowControl
{
	
	class SectionListener implements IExpansionListener
	{

		private Section lastExpanded;
		
		public void expansionStateChanged(ExpansionEvent e)
		{
//			Control controls[]=taskComposite.getChildren();
//			if(controls!=null)
//			{
//				for(Control c:controls)
//				{
//					c.dispose();
//				}
//			}
			tTBMng.removeAll();
			if(e.getState())
			{
				Object source=e.getSource();
				if(source instanceof Control)
				{
					Control c=(Control)source;
					List<TaskAction> actions=(List<TaskAction>)c.getData(KEY_ITASK_ACTIONS);
					if(actions==null)
					{
						IPhase p=(IPhase)c.getData(KEY_IPHASE);
						ArrayList<ITask> tasks= new ArrayList<ITask>();
						if(p!=null)
						{
							tasks.addAll(p.getTasks());
						}
						ITaskGroup tg=
							(ITaskGroup)c.getData(KEY_ITASKGROUP);
						if(tg!=null)
						{
							System.out.println("================="+tg.getTasks());
							for(ITask task:tg.getTasks())
							{
								tasks.add(task);
							}
						}
						
						ISubTaskGroup stg=
							(ISubTaskGroup)c.getData(KEY_ISUBTASKGROUP);
						if(stg!=null)
						{
							System.out.println("===STG=============="+stg.getTasks());
							for(ITask task:stg.getTasks())
							{
								tasks.add(task);
							}
						}
						///
						actions=new ArrayList<TaskAction>();
						TaskAction ta;
						for(ITask task:tasks)
						{
							System.out.println("ADDING:"+task);
							ta=new TaskAction(task);
							tTBMng.add(ta);
							actions.add(ta);
						}
						c.setData(KEY_ITASK_ACTIONS, actions);
					}
					else
					{
						for(TaskAction ta:actions)
						{
							tTBMng.add(ta);
						}
					}
					tTBMng.update(true);
					//tbMng.getControl().getParent().getParent().update();//redraw();
					taskComposite.reflow(false);
				}
				
			}
			tTBMng.update(true);
			taskComposite.reflow(false);
			
			if(lastExpanded!=e.getSource())
			{
				if(lastExpanded!=null)
				{
					Control cs[]=lastExpanded.getChildren();
					if(cs!=null)
					{
						for(Control c:cs)
						{
							if(c instanceof Section)
							{
								((Section)c).setExpanded(false);
							}
						}
					}
					lastExpanded.setExpanded(false);
	//				lastExpanded.redraw();
				}
				lastExpanded=(Section)e.getSource();
				form.reflow(false);
			}
			//top.update();
		}

		public void expansionStateChanging(ExpansionEvent e)
		{
			
		}		
		
		public Section getLastExpanded()
		{
			return lastExpanded;
		}
	};
	
	
	/////////////////////////////////////////////////////////////
	class TaskAction<E extends IWorkflowPart> extends Action
	{
		private E workflowPart;
		private List<TaskAction<IActivity>> activityActions;
//		public TaskAction(ITask task)
//		{
//			this.task=task;
//		}
		
		public TaskAction(E workflowPart)
		{
			this.workflowPart=workflowPart;
		}
		
		@Override
		public String getText()
		{
			return getWorkflowPartName(workflowPart);
		}
		
		@Override
		public void run()
		{
			System.out.println("RUNNING="+workflowPart);
			
			if(workflowPart instanceof ITask)
			{
				aTBMng.removeAll();
				if(activityActions==null)
				{
					activityActions= new ArrayList<TaskAction<IActivity>>();
					for(IActivity a: ((ITask)workflowPart).getActivities())
					{
						TaskAction<IActivity> action=new TaskAction<IActivity>(a);
						activityActions.add(action);
						aTBMng.add(action);
					}
					
				}
				else
				{
					//TODO nullPEx while using iterator throw compactloo
					for(int i=0;i<activityActions.size();i++)
					{
						aTBMng.add(activityActions.get(i));
					}
				}
				aTBMng.update(true);
				aTBComp.reflow(true);
			}
		}
		
		@Override
		public String getId()
		{
			return workflowPart.getURI();
		}
		@Override
		public int getStyle()
		{
			return Action.AS_PUSH_BUTTON;//super.getStyle();
		}
		
		public E getWorkflowPart()
		{
			return workflowPart;
		}
	};
	
	final static public String URI="_URI_";
	
	final static public String KEY_ITASK_ACTIONS="_KEY_ITASK_ACTIONS_";
	final static public String KEY_IPHASE=IPhase.class.toString();
	final static public String KEY_ITASKGROUP=ITaskGroup.class.toString();
	final static public String KEY_ISUBTASKGROUP=ISubTaskGroup.class.toString();
	final static public String KEY_ITASK=ITask.class.toString();
	
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
	private ScrolledForm aTBComp;
	private ScrolledForm taskComposite;
	
	private ToolBarManager tTBMng;
	private ToolBarManager aTBMng;
	ExpansionAdapter expansionAdapter=
		new ExpansionAdapter() {
			public void expansionStateChanged(ExpansionEvent e) {
				form.reflow(true);
			}
		};
	
	SectionListener stgEL = new SectionListener(); 
	SectionListener tgEL = new SectionListener(); 
	SectionListener pEL = new SectionListener();
	
	
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
			phase=(IPhase)ec.getData(KEY_IPHASE);
			Composite comp=toolkit.createComposite(ec,SWT.BORDER);
			comp.setLayout(new GridLayout());
			for(ITaskGroup tg:phase.getTaskGroups())
			{
				madeSec=createTaskGroupExpandable(tg,comp,phase);
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
			phase=(IPhase)sec.getData(KEY_IPHASE);
			taskGroup=(ITaskGroup)sec.getData(KEY_ITASKGROUP);
			Composite comp=toolkit.createComposite(sec,SWT.BORDER);
			comp.setLayout(new GridLayout());
			for(ISubTaskGroup stg:taskGroup.getSubTaskGroups())
			{
				madeSec=createSubTaskGroupExpandable(stg,comp,taskGroup,phase);
				madeSec.addExpansionListener(stgEL);
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
	
	private Composite contributeToStack(List<ITask> tasks)
	{
		Composite c=toolkit.createComposite(taskComposite);
		
		String name;
		c.setLayout(new GridLayout());
		for(ITask task:tasks)
		{
			name=getWorkflowPartName(task);
			toolkit.createButton(taskComposite,name,SWT.PUSH);
		}
		
		return c;
	}
	
	private void contributeToTasksPanel(List<ITask> tasks)
	{
		String name;
		taskComposite.setLayout(new GridLayout());
		for(ITask task:tasks)
		{
			name=getWorkflowPartName(task);
			toolkit.createButton(taskComposite,name,SWT.PUSH);
		}
		taskComposite.getParent().pack();
		taskComposite.redraw();
		
		//taskComposite.setVisible(true);
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
		ArrayList<Object> l=null;
		
		String name=getWorkflowPartName(t);
		Button b= toolkit.createButton(comp, name, SWT.NONE);
		return b;
	}

	private Section createSubTaskGroupExpandable(
									ISubTaskGroup stg, 
									Composite ec,
									ITaskGroup parentTG,
									IPhase phase)
	{
		Section childEC = 
		toolkit.createSection(
			ec, 
			Section.TREE_NODE|
			Section.CLIENT_INDENT|
			Section.TWISTIE);
		String name=getWorkflowPartName(stg);
		childEC.setText(name);
		childEC.setData(KEY_IPHASE, phase);
		childEC.setData(KEY_ISUBTASKGROUP, stg);
		childEC.setData(KEY_ITASKGROUP, parentTG);
		
		
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
		
		//sep activities
		Label al=toolkit.createSeparator(containerForm, SWT.VERTICAL|SWT.BOLD);
		fd= new FormData();
		fd.width=1;
		fd.left=new FormAttachment(form);
		//fd.right= new FormAttachment(90,0);
		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(0,0);
		al.setLayoutData(fd);
		//activities
		fd= new FormData();
		//fd.width=16;
		fd.left=new FormAttachment(al);//form);
		fd.right= new FormAttachment(100,0);
		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(0,0);
		aTBComp= 
			toolkit.createScrolledForm(containerForm);
		aTBComp.setLayoutData(fd);
		aTBComp.getBody().setLayout(new TableWrapLayout());
		ToolBar aTB= new ToolBar(
				aTBComp.getBody(),
				SWT.H_SCROLL|SWT.WRAP);
		aTBMng= new ToolBarManager(aTB);
		//Control tbC=tbMng.createControl(taskComposite);
		//toolkit.adapt(aTB);
//		Action a= new Action()
//		{
//		};
//		a.setText("DADADADAD");
//		aTBMng.add(a);
//		aTB.update();
		
		//SEPARAtor
		Label l=toolkit.createSeparator(containerForm, SWT.HORIZONTAL|SWT.BOLD);
		fd= new FormData();
		fd.left= new FormAttachment(0,0);
		fd.bottom= new FormAttachment(71,0);
		fd.top= new FormAttachment(form);//30,0);
		fd.right=new FormAttachment(al);//aTBComp);
		l.setLayoutData(fd);
		
		//tasks
		taskComposite=toolkit.createScrolledForm(containerForm);
//			toolkit.createComposite(
//					containerForm,
//					SWT.BORDER|SWT.BOLD);
		
		fd= new FormData();
		fd.left= new FormAttachment(0,0);
		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(l);//form);//30,0);
		fd.right=new FormAttachment(aTBComp);
		taskComposite.setLayoutData(fd);
		//taskComposite.setLayout(new FillLayout());
		//ScrolledForm cf=toolkit.createScrolledForm(taskComposite);
		//cf.getBody().setLayout(new TableWrapLayout());
		
		taskComposite.getBody().setLayout(new TableWrapLayout());
		ToolBar tb= new ToolBar(
					taskComposite.getBody(),
					SWT.V_SCROLL|SWT.WRAP);
		tTBMng= new ToolBarManager(tb);
		//Control tbC=tbMng.createControl(taskComposite);
		toolkit.adapt(tb);
		form.getBody().setLayout(new TableWrapLayout());
		
	}
	
	private Section createTaskGroupExpandable(
									ITaskGroup tg, 
									Composite ec,
									IPhase phase)
	{
		Section childEC = 
			toolkit.createSection(
						ec, 
						Section.TREE_NODE|
						Section.CLIENT_INDENT|
						Section.TWISTIE);
		
		childEC.setText(
				getWorkflowPartName(tg));
		childEC.setData(KEY_ITASKGROUP, tg);
		childEC.setData(KEY_IPHASE,phase);
		
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
		ec.setData(KEY_IPHASE, phase);
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
