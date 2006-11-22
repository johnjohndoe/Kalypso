package org.kalypso.afgui.viz;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.FlowLayout;
import org.eclipse.draw2d.LineBorder;
import org.eclipse.swt.graphics.Color;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.events.WorkflowChangeEvent;
import org.kalypso.afgui.model.events.WorkflowChangeEventListerner;

public class WorkflowFigure extends Figure
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
	    //ToolbarLayout layout = new ToolbarLayout();
	    FlowLayout fLayout= new FlowLayout();
	    fLayout.setHorizontal(false);
	    setLayoutManager(fLayout);	
	    setBorder(new LineBorder(ColorConstants.black,1));
	    setBackgroundColor(classColor);
	    setOpaque(true);
		
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
			IPhase[] phases =workflow.getPhases();
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
