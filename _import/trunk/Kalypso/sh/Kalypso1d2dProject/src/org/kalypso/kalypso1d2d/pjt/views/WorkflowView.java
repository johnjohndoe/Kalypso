package org.kalypso.kalypso1d2d.pjt.views;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.afgui.viz.WorkflowControl;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;
import org.kalypso.kalypsosimulationmodel.core.ISimulationModelProvider;
import org.kalypso.ogc.sensor.tableview.swing.actions.SetSelectedAction;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

import test.org.kalypso.afgui.TestRDFModel;

/**
 * @author Patrice Congo
 * 
 */
public class WorkflowView extends ViewPart implements ISimulationModelProvider
{
	private static final Logger logger = Logger.getLogger(WorkflowView.class);

	final static public String ID = "org.kalypso.kalypso1d2d.pjt.views.WorkflowView";

	private WorkflowControl workflowControl;

	private ActiveWorkContext activeWorkContext = ActiveWorkContext
			.getInstance();

	private IPartListener partListener = new IPartListener()
	{

		public void partActivated(IWorkbenchPart part)
		{
			// TODO Auto-generated method stub

		}

		public void partBroughtToTop(IWorkbenchPart part)
		{

		}

		public void partClosed(IWorkbenchPart part)
		{
			if (part instanceof SimulationModelDBView)
			{
				workflowControl.setVisible(true);
			}
		}

		public void partDeactivated(IWorkbenchPart part)
		{

		}

		public void partOpened(IWorkbenchPart part)
		{

		}

	};

	private IActiveContextChangeListener workContextChangeListener = new IActiveContextChangeListener()
	{

		public void activeProjectChanged(IProject newProject,
				IProject oldProject, IWorkflowDB oldDB,
				IWorkflowSystem oldWorkflowSystem)
		{
			IWorkflow workflow = activeWorkContext.getCurrentWorkflow();
			logger.info("New Workflow:" + workflow);
			workflowControl.setWorkflow(workflow, WorkflowView.this);
			workflowControl.setActiveProject(newProject);
			workflowControl.setVisible(false);

		}

	};

	private ISelectionListener workflowDataSelectionListener = new ISelectionListener()
	{

		public void selectionChanged(IWorkbenchPart part, ISelection selection)
		{
			if (part instanceof SimulationModelDBView)
			{
				if (selection.isEmpty())
				{
					workflowControl.setVisible(false);
				} else if (selection instanceof IStructuredSelection)
				{
					Object first = ((IStructuredSelection) selection)
							.getFirstElement();
					if (first instanceof IWorkflowData)
					{
						workflowControl.setVisible(true);
						// workflowControl.openEditor();
					} else
					{
						workflowControl.setVisible(false);
					}
				}

			}
		}

	};

	/**
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent)
	{
		// TODO change hard coding to test workflow
		// logger.warn("Using SH Test workflow");
		//		
		// workflowControl=
		// new WorkflowControl(TestRDFModel.getTesWorkflow());
		activeWorkContext
				.addActiveContextChangeListener(workContextChangeListener);
		workflowControl = new WorkflowControl(activeWorkContext
				.getCurrentWorkflow());
		workflowControl.createControl(parent);
		workflowControl.setVisible(false);
		ISelectionService selServ = getSite().getWorkbenchWindow()
				.getSelectionService();
		selServ.addPostSelectionListener(workflowDataSelectionListener);
		getSite().getWorkbenchWindow().getPartService().addPartListener(
				partListener);

	}

	/**
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus()
	{

	}

	/*
	 * TODO load model pool and also submodels
	 * 
	 */
	public IFeatureWrapper getSimulationModel()
	{
		final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

		final IProject activeProject = activeWorkContext.getActiveProject();
//		Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature
//				.toThisNature(activeProject);

//		IPoolableObjectType key = new PoolableObjectType("gml", "./bla", url);
//		IPoolListener l = new IPoolListener()
//		{
//
//			public void dirtyChanged(IPoolableObjectType key, boolean isDirty)
//			{
//				// TODO Auto-generated method stub
//
//			}
//
//			public boolean isDisposed()
//			{
//				// TODO Auto-generated method stub
//				return false;
//			}
//
//			public void objectInvalid(IPoolableObjectType key, Object oldValue)
//			{
//				// TODO Auto-generated method stub
//
//			}
//
//			public void objectLoaded(IPoolableObjectType key, Object newValue,
//					IStatus status)
//			{
//				// TODO Auto-generated method stub
//
//			}
//		};
//		pool.addPoolListener(l, key);

		return null;
	}

	/**
	 * 
	 * @see org.kalypso.kalypsosimulationmodel.core.ISimulationModelProvider#getSelection()
	 */
	public IStructuredSelection getSelection()
	{
		// TODO get selection
		return null;
	}

	/**
	 * @see org.kalypso.kalypsosimulationmodel.core.ISimulationModelProvider#getShell()
	 */
	public Shell getShell()
	{
		return getSite().getShell();
	}

}
