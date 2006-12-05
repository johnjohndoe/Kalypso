/**
 * 
 */
package test.org.kalypso.afgui;

import org.eclipse.draw2d.LightweightSystem;
import org.eclipse.draw2d.ScrollPane;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.FormColors;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.impl.WorkflowImpl;
import org.kalypso.afgui.viz.WorkflowControl;
import org.kalypso.afgui.viz.WorkflowFigure;

import com.hp.hpl.jena.rdf.model.Model;

/**
 * @author pat_dev
 *
 */
public class WorkflowFormTest
{
	static private Model model=TestRDFModel.getInstance().getShModel();
	static IWorkflow workflow=
		new WorkflowImpl(model.getResource(TestRDFModel.WORKFLOW_SH));
//	
//	static private Model model=TestRDFModel.getInstance().getWorkflowModel();
//	static IWorkflow workflow=
//		new WorkflowImpl(model.getResource(TestRDFModel.WORKFLOW1));
	
	public static void main(String args[]){
		Display d = new Display();
		final Shell shell = new Shell(d);
		shell.setSize(400, 400);
		shell.setText("UMLClassFigure Test");
		shell.setLayout(new FillLayout());
//		Composite comp;
//		FormToolkit toolkit = new FormToolkit(d);
//		
//		final ScrolledForm form = toolkit.createScrolledForm(shell);
//		form.setText("Sample form");
//		form.getBody().setLayout(new TableWrapLayout());
//		toolkit.createButton(form.getBody(), "Checkbox", SWT.CHECK);
//		//
//		toolkit.createSeparator(
//				form.getBody(), 
//				SWT.HORIZONTAL|SWT.FILL
//				).setBackground(
//						toolkit.getColors().getColor(FormColors.TB_TOGGLE_HOVER));
//		ExpandableComposite ec = toolkit.createExpandableComposite(form.getBody(), 
//				ExpandableComposite.TREE_NODE|
//				ExpandableComposite.CLIENT_INDENT|
//				ExpandableComposite.TWISTIE);
//		
//		ec.setText("Expandable Composite title");
//		String ctext = "We will now create a somewhat long text so that "+
//		"we can use it as content for the expandable composite. "+
//		"Expandable composite is used to hide or show the text using the "+
//		"toggle control";
//		
//		ec.setLayout(new TableWrapLayout());
//		
//		Button b=toolkit.createButton(ec, "BBB", SWT.NONE);
//		ec.setClient(b);
////		Label client = toolkit.createLabel(ec, ctext, SWT.WRAP);
////		ec.setClient(client);
//		TableWrapData td = new TableWrapData();
//		td.colspan = 1;
//		ec.setLayoutData(td);
//		ec.addExpansionListener(
//				new ExpansionAdapter() {
//						public void expansionStateChanged(ExpansionEvent e) {
//							form.reflow(true);
//						}
//				});
//		
//		///------------------------------------------------
//		toolkit.createSeparator(form.getBody(), SWT.HORIZONTAL|SWT.FILL);
//		ec = toolkit.createExpandableComposite(form.getBody(), 
//				ExpandableComposite.TREE_NODE|
//				ExpandableComposite.CLIENT_INDENT|
//				ExpandableComposite.TWISTIE);
//		Composite comp1= toolkit.createComposite(ec);
//		
//		ec.setText("Expandable Composite title1");		
//		ec.setLayout(new TableWrapLayout());
//		ec.setClient(comp1);
//		td.colspan = 1;
//		ec.setLayoutData(td);
//		
//		
//		b=toolkit.createButton(comp1, "BBB", SWT.NONE);
//		//ec.setClient(b);
//		td = new TableWrapData();
//		
//		
//		b=toolkit.createButton(comp1, "BBB2", SWT.NONE);
//		//ec.setClient(b);
////		Label client = toolkit.createLabel(ec, ctext, SWT.WRAP);
////		ec.setClient(client);
////		td = new TableWrapData();
////		td.colspan = 1;
////		ec.setLayoutData(td);
//		ec.addExpansionListener(
//				new ExpansionAdapter() {
//						public void expansionStateChanged(ExpansionEvent e) {
//							form.reflow(true);
//						}
//				});
		
		WorkflowControl wfc= new WorkflowControl(workflow);
		wfc.createControl(shell);
		shell.open();
		while (!shell.isDisposed())
			while (!d.readAndDispatch())
				d.sleep();
	 }
}
