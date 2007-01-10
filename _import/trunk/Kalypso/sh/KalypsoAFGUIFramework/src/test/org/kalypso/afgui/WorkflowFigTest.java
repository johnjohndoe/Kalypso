package test.org.kalypso.afgui;

import org.eclipse.draw2d.LightweightSystem;
import org.eclipse.draw2d.ScrollPane;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.viz.WorkflowFigure;


public class WorkflowFigTest
{
//	static private Model model=TestRDFModel.getInstance().getShModel();
//	static IWorkflow workflow=
//		new WorkflowImpl(model.getResource(TestRDFModel.WORKFLOW_SH));
		
	public static void main(String args[]){
		Display d = new Display();
		final Shell shell = new Shell(d);
		shell.setSize(400, 400);
		shell.setText("UMLClassFigure Test");
		
		LightweightSystem lws = 
			new LightweightSystem(shell);
//		Figure contents = new Figure();
//		Transposer t= new Transposer();
//		ScrollBarLayout sbl= new ScrollBarLayout(t);
//		contents.setLayoutManager(sbl);

//		FlowLayout flowLayout= new FlowLayout();
//		contents.setLayoutManager(flowLayout);
		
		//XYLayout contentsLayout = new XYLayout();
//		contents.setLayoutManager(contentsLayout);
		
		Font classFont = new Font(null, "Arial", 12, SWT.BOLD);
//		Label classLabel1 = new Label("Table", new Image(d, 
//			UMLClassFigureTest.class.getResourceAsStream("img/class_obj.gif")));
//		classLabel1.setFont(classFont);
//		
//		Label classLabel2 = new Label("Column", new Image(d, 
//		        UMLClassFigureTest.class.getResourceAsStream("img/class_obj.gif")));
//		classLabel2.setFont(classFont);
//		
//		final UMLClassFigure classFigure = new UMLClassFigure(classLabel1);
//		final UMLClassFigure classFigure2 = new UMLClassFigure(classLabel2);
//		
//		Label attribute1 = new Label("columns: Column[]", new Image(d, 
//			UMLClassFigure.class.getResourceAsStream("img/field_private_obj.gif")));
//		Label attribute2 = new Label("rows: Row[]", new Image(d, 
//			UMLClassFigure.class.getResourceAsStream("img/field_private_obj.gif")));
//		Label attribute3 = new Label("columnID: int", new Image(d, 
//			UMLClassFigure.class.getResourceAsStream("img/field_private_obj.gif")));
//		Label attribute4 = new Label("items: List", new Image(d, 
//			UMLClassFigure.class.getResourceAsStream("img/field_private_obj.gif")));
//
//		classFigure.getAttributesCompartment().add(attribute1);
//		classFigure.getAttributesCompartment().add(attribute2);
//		classFigure2.getAttributesCompartment().add(attribute3);
//		classFigure2.getAttributesCompartment().add(attribute4);
//
//		Label method1 = new Label("getColumns(): Column[]", new Image(d, 
//			UMLClassFigure.class.getResourceAsStream("img/methpub_obj.gif")));
//		Label method2 = new Label("getRows(): Row[]", new Image(d, 
//			UMLClassFigure.class.getResourceAsStream("img/methpub_obj.gif")));
//		Label method3 = new Label("getColumnID(): int", new Image(d, 
//			UMLClassFigure.class.getResourceAsStream("img/methpub_obj.gif")));
//		Label method4 = new Label("getItems(): List", new Image(d, 
//			UMLClassFigure.class.getResourceAsStream("img/methpub_obj.gif")));
//
//		classFigure.getMethodsCompartment().add(method1);
//		classFigure.getMethodsCompartment().add(method2);
//		classFigure2.getMethodsCompartment().add(method3);
//		classFigure2.getMethodsCompartment().add(method4);
//						
//		contentsLayout.setConstraint(classFigure, new Rectangle(10,10,-1,-1));
//		contentsLayout.setConstraint(classFigure2, new Rectangle(200, 200, -1, -1));
		
		
		
		
//		WorkflowFigure wfFig=
//			new WorkflowFigure(workflow);
//		contentsLayout.setConstraint(wfFig, new Rectangle(200, 200, -1, -1));
		//contents.add( wfFig	);
//		lws.setContents(contents);//contents);
		
//		lws.setContents(wfFig);
//		ScrollPane sp=new ScrollPane();
//		sp.setContents(wfFig);
//		lws.setContents(sp);
//		shell.open();
//		Button b6=new Button("B6");
//		wfFig.add(b6);
//		b6.setEnabled(false);

		

//		while (!shell.isDisposed())
//			while (!d.readAndDispatch())
//				d.sleep();
	 }
}
