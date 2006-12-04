package org.kalypso.afgui.viz;


import java.util.List;


import org.eclipse.draw2d.Button;
import org.eclipse.draw2d.Clickable;
import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.FlowLayout;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.LineBorder;
import org.eclipse.draw2d.RoundedRectangle;
import org.eclipse.draw2d.ToolbarLayout;
import org.eclipse.swt.graphics.Color;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.events.WorkflowChangeEvent;
import org.kalypso.afgui.model.events.WorkflowChangeEventListerner;

import sun.security.krb5.internal.tools.Klist;

public class WorkflowFigure extends WorkflowPartFigure<IWorkflow> //Figure
{
	public static Color classColor = new Color(null,255,255,206);
	
//	private Button b1= new Button("B1");
//	private Button b2= new Button("B2");
//	private Button b3= new Button("B3");
//	private Button b4= new Button("B4");
	
	/**
	 * The worflow which is being vizialized
	 */
	private IWorkflow workflow;
	
	/**
	 * Listen to workflow events and update the visualisation
	 */
	private WorkflowFigUpdater figUpdater=
							new WorkflowFigUpdater();
	
	
	public WorkflowFigure(IWorkflow workflow) 
	{
		super(workflow,IWorkflow.class);
//	    ToolbarLayout tbLayout = new ToolbarLayout();
//	    tbLayout.setMinorAlignment(ToolbarLayout.ALIGN_CENTER);
//	    tbLayout.setStretchMinorAxis(true);
//	    Figure fig= new Clickable(new RoundedRectangle());
//	    fig.setMinimumSize(getPreferredSize());
//	    
////	    FlowLayout fLayout= new FlowLayout();
////	    fLayout.setHorizontal(false);
////	    setLayoutManager(fLayout);
//	    setLayoutManager(tbLayout);
//	    setBorder(new LineBorder(ColorConstants.black,1));
//	    setBackgroundColor(classColor);
//	    setOpaque(true);
//	    add(new Label(workflow.getName()));
	    //add(fig);
	    List<IPhase> phases=workflow.getPhases();
	    //IFigure comp=new CompartmentFigure();
    	
	    //comp.setLayoutManager(new FlowLayout(false));
	    //add(comp);
	    for(IPhase p:phases)
	    {
	    	System.out.println("Fig for phase:"+p);
	    	//CompartmentFigure cf=new CompartmentFigure(p);
	    	
	    	WorkflowPartFigure<IPhase> cf=add(p,IPhase.class);
	    	for(ITaskGroup tg:p.getTaskGroups())
	    	{
	    		WorkflowPartFigure<ITaskGroup> tgCF=cf.add(tg,ITaskGroup.class);
	    		for(ISubTaskGroup stg:tg.getSubTaskGroups())
	    		{
	    			WorkflowPartFigure<ISubTaskGroup> stgCF=tgCF.add(stg,ISubTaskGroup.class);
	    			for(ITask t:stg.getTasks())
	    			{
	    				WorkflowPartFigure<ITask> tCF=stgCF.add(t,ITask.class);
	    			}
	    		}
	    			
	    	}
	    	//comp.add(new Label(p.getName()));
//	    	comp.add(new PhaseFig(p));
//	    	comp.add(new Button("B_"+p.getName()));
	    }
	    
		
//	    add(name);	
//	    //add(attributeFigure);
//	    //add(methodFigure);
//	    add(b1);
//	    add(b2);
//	    add(b3);
//	    add(b4);
	}
	
	/**
	 * Update the workflow appearance
	 */
	private void update()
	{
		removeAll();
		if(workflow==null)
		{
			return;
		}
		else
		{
			List<IPhase> phases =workflow.getPhases();
			for(IPhase phase:phases)
			{
				add(new PhaseFig(phase));
			}
		}
	}
	
	public void setWorkflow(IWorkflow nextWorkflow)
	{
		if(workflow!=null)
		{
			this.workflow.removeWorkflowChangedEventListener(figUpdater);
		}
		this.workflow=nextWorkflow;
		update();		
	}
	
	public  IWorkflow  getWorkflow()
	{
		return workflow;
	}		
	
	class WorkflowFigUpdater implements  WorkflowChangeEventListerner
	{

		public void onWorkflowChanged(WorkflowChangeEvent event)
		{
			update();
		}
		
	}
}
