package org.kalypso.afgui.viz;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
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
import org.kalypso.afgui.model.impl.TaskGroup;

public class WorkflowControl
{
	final static public String URI="_URI_"; 
	private IWorkflow workflow;
	
	private Composite top;
	
	private FormToolkit toolkit;
	private ScrolledForm form;
	ExpansionAdapter expansionAdapter=
		new ExpansionAdapter() {
			public void expansionStateChanged(ExpansionEvent e) {
				form.reflow(true);
			}
		};
	
	public WorkflowControl(IWorkflow workflow)
	{
		this.workflow=workflow;
	}
	
	public void createControl(Composite parent)
	{
		top = new Composite(parent,SWT.FILL);
		top.setLayout(new FillLayout());
		toolkit= new FormToolkit(top.getDisplay());
		
		form = 
			toolkit.createScrolledForm(top);
		form.getBody().setLayout(new TableWrapLayout());
		List<Section> phaseCntls=
			new ArrayList<Section>();
		Section madeSec;
		for(IPhase phase:workflow.getPhases())
		{
			madeSec=createPhaseExpandable(phase,form);
			phaseCntls.add(madeSec);
			toolkit.createCompositeSeparator(madeSec);
		}
				
		List<Section> taskGroupECs=
					Collections.synchronizedList(new ArrayList<Section>());
		
		IPhase phase;
		for(Section ec:phaseCntls)
		{
			phase=(IPhase)ec.getData(URI);
			Composite comp=toolkit.createComposite(ec);
			comp.setLayout(new GridLayout());
			for(ITaskGroup tg:phase.getTaskGroups())
			{
				madeSec=createTaskGroupExpandable(tg,comp);
				taskGroupECs.add(madeSec);
				toolkit.createCompositeSeparator(madeSec);
			}
			ec.setClient(comp);
		}
		
		
		///SubTaskGroup
		ITaskGroup taskGroup;
		List<Section> stgSecs= new ArrayList<Section>(); 
		for(Section sec:taskGroupECs)//;int i=0;i<taskGroupECs.size();i++)
		{
			//System.out.println("III="+i);
			
			taskGroup=(ITaskGroup)sec.getData(URI);
			Composite comp=toolkit.createComposite(sec);
			comp.setLayout(new GridLayout());
			for(ISubTaskGroup stg:taskGroup.getSubTaskGroups())
			{
				madeSec=createSubTaskGroupExpandable(stg,comp);
				stgSecs.add(madeSec);
			}
			sec.setClient(comp);
		}
		
//		/ITask
		ISubTaskGroup subTaskGroup;
		List<Button> taskButtons= new ArrayList<Button>(); 
		for(Section sec:stgSecs)//;int i=0;i<taskGroupECs.size();i++)
		{
			//System.out.println("III="+i);
			
			subTaskGroup=(ISubTaskGroup)sec.getData(URI);
			Composite comp=toolkit.createComposite(sec);
			comp.setLayout(new GridLayout());
			for(ITask t:subTaskGroup.getTasks())
			{
				Button taskButton=createTaskButton(t,comp);
				taskButtons.add(taskButton);
			}
			sec.setClient(comp);
		}
	}

	
	private Button createTaskButton(ITask t, Composite comp)
	{
		String name=t.getName();
		if(name==null)
		{
			name=t.getURI();
		}
		Button b= toolkit.createButton(comp, t.getName(), SWT.NONE);
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
		String name=stg.getName();
		childEC.setText(
				(name==null)?stg.toString():name);
		childEC.setData(URI, stg);
		
		return childEC;
		
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
		
		childEC.setText(tg.getName());
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
		ec.setText(phase.getName());
		ec.setData(URI, phase);
		//ec.setLayout(new TableWrapLayout());
		return ec;
	}
	
}
