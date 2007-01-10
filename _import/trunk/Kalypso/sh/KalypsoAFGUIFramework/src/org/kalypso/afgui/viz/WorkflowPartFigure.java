/**
 * 
 */
package org.kalypso.afgui.viz;

import java.util.Hashtable;
import java.util.Map;

import org.eclipse.draw2d.BorderLayout;
import org.eclipse.draw2d.ButtonBorder;
import org.eclipse.draw2d.Clickable;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.RoundedRectangle;
import org.eclipse.draw2d.ToolbarLayout;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.draw2d.geometry.Insets;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowPart;

public class WorkflowPartFigure<T extends IWorkflowPart> extends Clickable// Figure
{
	static final Map<Class, Font> MAP_FONT = new Hashtable<Class, Font>();

	static {
		MAP_FONT.put(IWorkflow.class, new Font(null, "Arial", 11, SWT.BOLD));
		// MAP_FONT.put(IPhase.class, new Font(null, "Arial", 10, SWT.BOLD));
		// MAP_FONT.put(ITaskGroup.class, new Font(null, "Arial", 9, SWT.BOLD));
		// MAP_FONT.put(ISubTaskGroup.class, new Font(null, "Arial", 8,
		// SWT.BOLD));
		MAP_FONT.put(ITask.class, new Font(null, "Arial", 10, SWT.BOLD));
		MAP_FONT.put(IActivity.class, new Font(null, "Arial", 8, SWT.BOLD));
	};

	T wfPart;

	IFigure childrenFolder;

	Class type;

	public WorkflowPartFigure(IWorkflowPart part, Class<T> type) {
		super();
		this.type = type;

		ToolbarLayout layout = new ToolbarLayout();
		layout.setMinorAlignment(ToolbarLayout.ALIGN_CENTER);
		layout.setStretchMinorAxis(true);
		layout.setSpacing(2);
		setLayoutManager(layout);
		setBorder(new CompartmentFigureBorder());

		Label label = new Label(part.getName());
		label.setFont(getClassFont(type));
		add(label, BorderLayout.TOP);

		childrenFolder = new RoundedRectangle();

		layout = new ToolbarLayout(false);
		layout.setStretchMinorAxis(true);
		layout.setSpacing(10);
		setOpaque(false);

		childrenFolder.setLayoutManager(layout);

		add(childrenFolder, BorderLayout.CENTER);
		Figure pad = new Figure();
		pad.setMinimumSize(new Dimension(10, 100));
		// add(new Button("b"), BorderLayout.LEFT);

		// setContents(childrenFolder);
		if (part instanceof IWorkflow) {
			childrenFolder.setVisible(true);
		}
		// else if(part instanceof IPhase)
		// {
		// childrenFolder.setVisible(true);
		// }
		// else if(part instanceof ITaskGroup)
		// {
		// childrenFolder.setVisible(true);
		// }
		// else if(part instanceof ISubTaskGroup)
		// {
		// childrenFolder.setVisible(true);
		// }
		else if (part instanceof ITask) {
			FontData[] fontData = label.getFont().getFontData();
			if (fontData.length > 0) {
				fontData[0].setHeight(9);
			}
			childrenFolder.setVisible(true);
		} else {
			childrenFolder.setVisible(false);
		}

	}

	final static private Font getClassFont(Class type) {
		return MAP_FONT.get(type);
	}

	public <E extends IWorkflowPart> WorkflowPartFigure<E> add(E childPart,
			Class<E> type) {
		System.out.println("adding=" + childPart);
		WorkflowPartFigure<E> cf = new WorkflowPartFigure<E>(childPart, type);
		childrenFolder.add(cf);
		return cf;
	}

	public class CompartmentFigureBorder extends ButtonBorder// AbstractBorder
	{
		public CompartmentFigureBorder() {

		}

		public Insets getInsets(IFigure figure) {
			return new Insets(0, 20, 10, 10);
		}

		// public void paint(IFigure figure, Graphics graphics, Insets insets)
		// {
		// graphics.drawLine(getPaintRectangle(figure, insets).getTopLeft(),
		// tempRect.getTopRight());
		// }
	}
}