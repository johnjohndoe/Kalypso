package org.kalypso.afgui.viz;

import java.util.List;

import org.eclipse.draw2d.BorderLayout;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.FlowLayout;
import org.eclipse.draw2d.IFigure;
import org.eclipse.swt.graphics.Color;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;

public class WorkflowFigure extends Figure {
	public static Color classColor = new Color(null, 255, 255, 106);

	private IFigure phaseBar;

	/**
	 * The worflow which is being vizialized
	 */
	private IWorkflow workflow;

	public WorkflowFigure(IWorkflow workflow) {
		super();
		this.workflow = workflow;

		BorderLayout bl = new BorderLayout();
		bl.setHorizontalSpacing(2);
		bl.setVerticalSpacing(2);
		setLayoutManager(bl);

		// phase bar
		configPhaseBar();
	}

	final private void configPhaseBar() {
		List<ITaskGroup> phases = workflow.getTaskGroups();
		phaseBar = new Figure();
		FlowLayout fl = new FlowLayout();
		fl.setStretchMinorAxis(true);
		phaseBar.setLayoutManager(fl);
		phaseBar.setVisible(true);
		PhaseFig phaseFig;
		for (ITaskGroup phase : phases) {
			phaseFig = new PhaseFig(phase);
			phaseBar.add(phaseFig);
		}
		add(phaseBar, BorderLayout.TOP);
	}

	/**
	 * Update the workflow appearance
	 */
	private void update() {
		removeAll();
		if (workflow == null) {
			return;
		} else {
			List<ITaskGroup> phases = workflow.getTaskGroups();
			for (ITaskGroup phase : phases) {
				add(new PhaseFig(phase));
			}
		}
	}

	public void setWorkflow(IWorkflow nextWorkflow) {
		this.workflow = nextWorkflow;
		update();
	}

	public IWorkflow getWorkflow() {
		return workflow;
	}
}
