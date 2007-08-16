package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.actions.ScenarioHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class ScenarioResultNavigatorContentProvider extends WorkbenchContentProvider implements ITreeContentProvider
{
  @Override
  public Object[] getChildren( Object parentElement )
  {
    if( parentElement instanceof Scenario )
    {
      final Scenario scenario = (Scenario) parentElement;
      final IFolder scenarioFolder = ScenarioHelper.getFolder( scenario );

      final IHandlerService service = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();
      // final Shell shell = (Shell) currentState.getVariable( ISources.ACTIVE_SHELL_NAME );
      final IFolder activeFolder = (IFolder) currentState.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
      final ICaseDataProvider<IFeatureWrapper2> caseDataProvider = (ICaseDataProvider<IFeatureWrapper2>) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      if( scenarioFolder.equals( activeFolder ) )
      {
        try
        {
          final IScenarioResultMeta model = caseDataProvider.getModel( IScenarioResultMeta.class );
          if( model != null )
            return new Object[] { model };
        }
        catch( CoreException e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }
    }

    return super.getChildren( parentElement );
  }

  @Override
  public boolean hasChildren( Object element )
  {
    if( element instanceof Scenario )
      return getChildren( element ).length > 0;

    return super.hasChildren( element );
  }

}
