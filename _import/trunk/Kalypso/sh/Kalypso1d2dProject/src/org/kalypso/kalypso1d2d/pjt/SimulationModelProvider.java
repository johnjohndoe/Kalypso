package org.kalypso.kalypso1d2d.pjt;

import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.AbstractSourceProvider;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.kalypso1d2d.pjt.views.WorkflowView;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;
import org.kalypso.kalypsosimulationmodel.core.ISimulationModelProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.ResourcePool;

public class SimulationModelProvider extends AbstractSourceProvider implements
        ISimulationModelProvider {

    private static final Logger LOGGER = Logger.getLogger(WorkflowView.class
            .getName());

    static {
        final boolean log = Boolean.parseBoolean(Platform
                .getDebugOption("org.kalypso.kalypso1d2d.pjt/debug"));
        if (log)
            LOGGER.setUseParentHandlers(false);
    }

    private static final String[] PROVIDED_SOURCE_NAMES = new String[] {
            ISimulationModelProvider.ACTIVE_SIMULATION_MODEL_BASE_FOLDER_NAME,
            ISimulationModelProvider.ACTIVE_SIMULATION_MODEL_NAME };

    protected ActiveWorkContext activeWorkContext;

    private IActiveContextChangeListener workContextChangeListener = new IActiveContextChangeListener() {

        public void activeProjectChanged(IProject newProject,
                IProject oldProject, IWorkflowDB oldDB,
                IWorkflowSystem oldWorkflowSystem) {
            fireSourceChanged(0, getCurrentState());
        }

    };

    public SimulationModelProvider(final ActiveWorkContext context) {
        activeWorkContext = context;
        activeWorkContext
                .addActiveContextChangeListener(workContextChangeListener);
    }

    /**
     * @see org.eclipse.ui.ISourceProvider#dispose()
     */
    public void dispose() {
        activeWorkContext = null;
    }

    /**
     * @see org.eclipse.ui.ISourceProvider#getCurrentState()
     */
    public Map getCurrentState() {
        final Map currentState = new TreeMap();
        currentState
                .put(
                        ISimulationModelProvider.ACTIVE_SIMULATION_MODEL_BASE_FOLDER_NAME,
                        getSimulationModelBase());
        currentState.put(ISimulationModelProvider.ACTIVE_SIMULATION_MODEL_NAME,
                getSimulationModel());
        return currentState;
    }

    /**
     * @see org.eclipse.ui.ISourceProvider#getProvidedSourceNames()
     */
    public String[] getProvidedSourceNames() {
        return PROVIDED_SOURCE_NAMES;
    }

    private IFeatureWrapper getSimulationModel() {
        // TODO load model pool and also submodels
        final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

        final IProject activeProject = activeWorkContext.getActiveProject();
        // Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature
        // .toThisNature(activeProject);

        // IPoolableObjectType key = new PoolableObjectType("gml", "./bla",
        // url);
        // IPoolListener l = new IPoolListener()
        // {
        //
        // public void dirtyChanged(IPoolableObjectType key, boolean isDirty)
        // {
        // // TODO Auto-generated method stub
        //
        // }
        //
        // public boolean isDisposed()
        // {
        // // TODO Auto-generated method stub
        // return false;
        // }
        //
        // public void objectInvalid(IPoolableObjectType key, Object oldValue)
        // {
        // // TODO Auto-generated method stub
        //
        // }
        //
        // public void objectLoaded(IPoolableObjectType key, Object newValue,
        // IStatus status)
        // {
        // // TODO Auto-generated method stub
        //
        // }
        // };
        // pool.addPoolListener(l, key);

        return null;
    }

    private IFolder getSimulationModelBase() {
        final IProject activeProject = activeWorkContext.getActiveProject();
        // final String simModelLocation = null;
        // m_workflowData.getLocation();
        // LOGGER.info("simModelLocation = " + simModelLocation);
        // final IResource modelBase =
        // activeProject.findMember(simModelLocation);
        // if (modelBase.getType() == IResource.FOLDER)
        // return (IFolder) modelBase;
        // else
        if (activeProject != null)
            return activeProject.getFolder(activeProject.getLocation());
        else
            return null;
    }

}
