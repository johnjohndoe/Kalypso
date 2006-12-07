package org.kalypso.afgui.viz;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.swing.text.DefaultEditorKit.CutAction;



import org.apache.log4j.Logger;
import org.eclipse.core.internal.resources.File;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.draw2d.FlowLayout;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.Separator;
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
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
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
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.views.contentoutline.ContentOutline;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IHelp;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.afgui.model.impl.SubTaskGroup;
import org.kalypso.afgui.model.impl.TaskGroup;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ui.editor.mapeditor.GisMapOutlinePage;

import EDU.oswego.cs.dl.util.concurrent.Takable;

public class WorkflowControl
{
	public static final Logger logger= 
				Logger.getLogger(WorkflowControl.class);
	
	public static final String KEY_GROUP_TASKS="_GROUP_TASKS";
	public static final String KEY_GROUP_ACTIVITIES="_GROUP_ACTIVITIES";
	/////////////////////////////////////////////////
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
			IContributionItem ci=null;//aTBMng.
			
			tTBMng.removeAll();
			aTBMng.removeAll();
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
							logger.info("================="+tg.getTasks());
							for(ITask task:tg.getTasks())
							{
								tasks.add(task);
							}
						}
						
						ISubTaskGroup stg=
							(ISubTaskGroup)c.getData(KEY_ISUBTASKGROUP);
						if(stg!=null)
						{
							logger.info("===STG=============="+stg.getTasks());
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
							logger.info("ADDING:"+task);
							ta=new TaskAction(task);
							tTBMng.add(ta);
							//tTBMng.getControl().
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
					tTBComp.reflow(true);
				}
				
			}
			tTBMng.update(true);
			tTBComp.reflow(false);
			aTBMng.update(true);
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
			logger.info("RUNNING="+workflowPart);
			if(workflowPart instanceof ITask)
			{
				aTBMng.removeAll();
				new Label(
						aTBMng.getControl(),
						SWT.BORDER).setText(((ITask)workflowPart).getName());
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
			
			String uri=workflowPart.getURI();
			doURITask(uri);
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
		
		
		
		@Override
		public String getToolTipText()
		{
			try
			{
				return workflowPart.getHelp().getHelp();
			}
			catch(Throwable th)
			{
				return null;
			}
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
	private ScrolledForm tTBComp;
	
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
	IProject activeProject;
	
	
	public WorkflowControl(IWorkflow workflow)
	{
		this.workflow=workflow;
	}
	
	public void createControl(Composite parent)
	{
		//top
		top = new Composite(parent,SWT.FILL);
		top.setLayout(new FillLayout());
		
		//createBaseContainers(parent);
		createBaseContainersBottomToolbars(parent);
		createWorkFlowView();
	}
	
	public void setActiveProject(IProject activeProject)
	{
		this.activeProject = activeProject;
	}
	
	public void setWorkflow(IWorkflow workflow)
	{
		this.workflow = workflow;
		createWorkFlowView();
		form.reflow(true);
		tTBComp.reflow(true);
		aTBComp.reflow(true);
		
	}
	
	public void setVisible(boolean visible)
	{
		top.setVisible(visible);
		top.getParent().update();
		tTBMng.removeAll();
		tTBMng.update(true);
		aTBMng.removeAll();
		aTBMng.update(true);
//		this.workflow = workflow;
//		createWorkFlowView();
//		form.reflow(true);
//		tTBComp.reflow(true);
//		aTBComp.reflow(true);
		
	}
	
	Map<Object, List<Control>> taskControlMap;
	
	private void createWorkFlowView()
	{
		//remove old layout element in form
		for(Control c:form.getBody().getChildren())
		{
			//recycle getData(key)
			c.dispose();
		}
		aTBMng.removeAll();
		tTBMng.removeAll();
		if(workflow==null)
		{
			return;
		}
		
//		/IPhase
		phaseCntls=
			new ArrayList<Section>();
		Section madeSec;
		
		//twd.rowspan=TableWrapData.FILL;
		//form.getBody().setLayout(new TableWrapLayout());
		form.getBody().setLayout(new FormLayout());
		FormData fd= new FormData();
		fd.left= new FormAttachment(0,0);
		fd.right= new FormAttachment(100,0);
		fd.top=new FormAttachment(0,0);
		
		for(IPhase phase:workflow.getPhases())
		{
			madeSec=createPhaseExpandable(phase,form);
			phaseCntls.add(madeSec);
			madeSec.setLayoutData(fd);
			fd= new FormData();
			fd.left= new FormAttachment(0,0);
			fd.right= new FormAttachment(100,0);
			fd.top=new FormAttachment(madeSec);
			madeSec.addExpansionListener(pEL);
			//toolkit.createCompositeSeparator(madeSec);
			
		}
				
		taskGroupECs=
					new ArrayList<Section>();
		
		IPhase phase;
		List<ITaskGroup> curTGList;
		for(Section ec:phaseCntls)
		{
			phase=(IPhase)ec.getData(KEY_IPHASE);
			curTGList= phase.getTaskGroups();
			if(curTGList.isEmpty())
			{
				//empty
			}
			else
			{
				Composite comp=toolkit.createComposite(ec,SWT.BORDER);
				comp.setLayout(new GridLayout());
				for(ITaskGroup tg:phase.getTaskGroups())
				{
					madeSec=createTaskGroupExpandable(tg,comp,phase);
					taskGroupECs.add(madeSec);
					madeSec.addExpansionListener(tgEL);
					//toolkit.createCompositeSeparator(madeSec);
				}
				ec.setClient(comp);
			}
		}
		
		
		///SubTaskGroup
		ITaskGroup taskGroup;
		List<ISubTaskGroup> curSTGList;
		stgSecs= new ArrayList<Section>(); 
		for(Section sec:taskGroupECs)//;int i=0;i<taskGroupECs.size();i++)
		{
			//logger.info("III="+i);
			taskGroup=(ITaskGroup)sec.getData(KEY_ITASKGROUP);
			curSTGList=taskGroup.getSubTaskGroups();
			if(curSTGList.isEmpty())
			{
				//empty
			}
			else
			{
				phase=(IPhase)sec.getData(KEY_IPHASE);				
				Composite comp=toolkit.createComposite(sec,SWT.BORDER);
				comp.setLayout(new GridLayout());
				for(ISubTaskGroup stg:curSTGList)
				{
					madeSec=createSubTaskGroupExpandable(stg,comp,taskGroup,phase);
					madeSec.addExpansionListener(stgEL);
					stgSecs.add(madeSec);
				}
				sec.setClient(comp);
			}
		}
	}
	
	private Composite contributeToStack(List<ITask> tasks)
	{
		Composite c=toolkit.createComposite(tTBComp);
		
		String name;
		c.setLayout(new GridLayout());
		for(ITask task:tasks)
		{
			name=getWorkflowPartName(task);
			toolkit.createButton(tTBComp,name,SWT.PUSH);
		}
		
		return c;
	}
	
	private void contributeToTasksPanel(List<ITask> tasks)
	{
		String name;
		tTBComp.setLayout(new GridLayout());
		for(ITask task:tasks)
		{
			name=getWorkflowPartName(task);
			toolkit.createButton(tTBComp,name,SWT.PUSH);
		}
		tTBComp.getParent().pack();
		tTBComp.redraw();
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
		fd.bottom= new FormAttachment(100,0);
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
		tTBComp=toolkit.createScrolledForm(containerForm);
//			toolkit.createComposite(
//					containerForm,
//					SWT.BORDER|SWT.BOLD);
		
		fd= new FormData();
		fd.left= new FormAttachment(0,0);
		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(l);//form);//30,0);
		fd.right=new FormAttachment(aTBComp);
		tTBComp.setLayoutData(fd);
		//taskComposite.setLayout(new FillLayout());
		//ScrolledForm cf=toolkit.createScrolledForm(taskComposite);
		//cf.getBody().setLayout(new TableWrapLayout());
		
		tTBComp.getBody().setLayout(new TableWrapLayout());
		ToolBar tb= new ToolBar(
					tTBComp.getBody(),
					SWT.V_SCROLL|SWT.WRAP|SWT.VERTICAL);
		tTBMng= new ToolBarManager(tb);
		tTBMng.add(new GroupMarker(KEY_GROUP_TASKS));
		//Group g= new Group(tb,SWT.NONE);
		
		tTBMng.add(new GroupMarker(KEY_GROUP_ACTIVITIES));
		//Control tbC=tbMng.createControl(taskComposite);
		toolkit.adapt(tb);
		form.getBody().setLayout(new TableWrapLayout());
		
	}
	
	private void createBaseContainersBottomToolbars(Composite parent)
	{
		
//		top = new Composite(parent,SWT.FILL);
//		top.setLayout(new FillLayout());
		toolkit= new FormToolkit(top.getDisplay());
		Composite containerForm=toolkit.createComposite(top);
		
		
			
		containerForm.setLayout(new FormLayout());
		
		FormData fd;
		
		fd= new FormData();
		fd.width=270;//TODO check how not to use width
		fd.left= new FormAttachment(0,0);
		fd.bottom= new FormAttachment(60,0);
		fd.top= new FormAttachment(0,0);
		form = 
			toolkit.createScrolledForm(containerForm);
		form.setLayoutData(fd);
		
//		SEPARAtor workflow (task activities)
		Label wSepTA=toolkit.createSeparator(
					containerForm, SWT.HORIZONTAL|SWT.BOLD);
		fd= new FormData();
		fd.height=1;
		fd.left= new FormAttachment(0,0);
//		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(form);//30,0);
		fd.right=new FormAttachment(100,0);//aTBComp);
		wSepTA.setLayoutData(fd);
		
//		tasks bottom left
		tTBComp=toolkit.createScrolledForm(containerForm);
//			toolkit.createComposite(
//					containerForm,
//					SWT.BORDER|SWT.BOLD);
		
		fd= new FormData();
		fd.left= new FormAttachment(0,0);
		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(wSepTA);//form);//30,0);
		fd.right=new FormAttachment(50,0);
		tTBComp.setLayoutData(fd);
		
//		SEPARAtor task activities
		Label l=toolkit.createSeparator(
				containerForm, SWT.VERTICAL|SWT.BOLD);
		fd= new FormData();
		fd.width=1;
		fd.left= new FormAttachment(tTBComp);
		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(form);//30,0);
		//fd.right=new FormAttachment(al);//aTBComp);
		l.setLayoutData(fd);
		
		//activities
		fd= new FormData();
		//fd.width=16;
		fd.left=new FormAttachment(l);//form);
		fd.right= new FormAttachment(100,0);
		fd.bottom= new FormAttachment(100,0);
		fd.top= new FormAttachment(form);
		aTBComp= 
			toolkit.createScrolledForm(containerForm);
		aTBComp.setLayoutData(fd);
		aTBComp.getBody().setLayout(new TableWrapLayout());
		ToolBar aTB= new ToolBar(
				aTBComp.getBody(),
				SWT.V_SCROLL|SWT.WRAP|SWT.VERTICAL);
		aTBMng= new ToolBarManager(aTB);
		toolkit.adapt(aTB);
		
		
		//taskComposite.setLayout(new FillLayout());
		//ScrolledForm cf=toolkit.createScrolledForm(taskComposite);
		//cf.getBody().setLayout(new TableWrapLayout());
		
		tTBComp.getBody().setLayout(new TableWrapLayout());
		ToolBar tb= new ToolBar(
					tTBComp.getBody(),
					SWT.V_SCROLL|SWT.WRAP|SWT.VERTICAL);
		
		tTBMng= new ToolBarManager(tb);
		//Control tbC=tbMng.createControl(taskComposite);
		toolkit.adapt(tb);
		
		//form.getBody().setLayout(new TableWrapLayout());
		
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
						Section.TWISTIE|Section.DESCRIPTION|
						Section.TITLE_BAR);
		
		childEC.setText(
				getWorkflowPartName(tg));
		childEC.setToolTipText(getWorkflowPartHelp(tg));
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
						Section.TWISTIE|Section.DESCRIPTION|
						Section.TITLE_BAR
						);
		ec.setText(
				getWorkflowPartName(phase));
		ec.setToolTipText(getWorkflowPartHelp(phase));
		
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
	
	final static public String getWorkflowPartHelp(IWorkflowPart wp)
	{
		IHelp  help=wp.getHelp();
		
		if(help==null)
		{
			return "";
		}
		else
		{
			String helpString=help.getHelp();
			if(helpString==null)
			{
				return "";
			}
			else
			{
				return helpString;
			}
		}
	}
	
	public static final String LOAD_URI=
		"http://www.tu-harburg.de/wb/kalypso/kb/workflow/test#MapLoad";
	public static final String LOAD_K2D2D=
		"http://www.tu-harburg.de/wb/kalypso/kb/workflow/test#LoadK2D2D";
	public static final String LOAD_IMG_TIFF=
		"http://www.tu-harburg.de/wb/kalypso/kb/workflow/test#LoadTiff";
	public static final String LOAD_IMG_JPG=
		"http://www.tu-harburg.de/wb/kalypso/kb/workflow/test#LoadJPEG";
	
	private IWorkbenchPage page;
	private GisMapOutlineViewer m_outlineviewer;
	
	private void resolveGisMapOutlineViewer()
	{
//		for(IWorkbenchPage p:PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPages())
//		{
//			ContentOutline co;
//			IViewPart vp=p.findView(IPageLayout.ID_OUTLINE);
//			vp.get
//			
//		}
		
		//go through editor
		//IContentOutlinePage outlinePage = (IContentOutlinePage) editor.getAdapter(IContentOutlinePage.class);
	}
	
	void openEditor()
	{
		logger.info("Opening editor");
		if(page==null)
		{
			page = 
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		}
		if(page!=null)
		{
		   
			try
			{
				if(activeProject==null)
				{
					logger.warn("Active project is null");
					return ;
				}
				IWorkspace ws=ResourcesPlugin.getWorkspace();
				
				IPath gmtPath=new Path("/.metadata/agger_karte.gmt");
				IFile gmtFile=
					activeProject.getFile("project:/.metadata/agger_karte.gmt");
				if(!gmtFile.exists())
				{
					logger.warn(
							"DO NOT EXISTS:"+gmtFile+
							" pjt="+activeProject);
					return;
				}
				
				IEditorPart ep=IDE.openEditor(
						page, 
						gmtFile);
				GisMapOutlinePage gmoPage=
					(GisMapOutlinePage)ep.getAdapter(IContentOutlinePage.class);
				
				//logger.info("ContentOutLine="+ep.getAdapter(IContentOutlinePage.class));
				//GisMapOutlinePage getModelView for  setting themes
			}
			catch (PartInitException e)
			{
				logger.error("/test/Karte.gmt",e);
			}
//		   IEditorDescriptor desc = PlatformUI.getWorkbench().
//		      getEditorRegistry().getDefaultEditor(file.getName());
//		   page.openEditor(
//		      new FileEditorInput(file),
//		      desc.getId());
		}
		
	}
	
	private final void doURITask(String uri)
	{
		if(uri==null)
		{
			logger.warn("uri task is null");
		}
		else
		{
			if(LOAD_URI.equals(uri))
			{
				openEditor();
			}
		}		
	}
	
	
}
