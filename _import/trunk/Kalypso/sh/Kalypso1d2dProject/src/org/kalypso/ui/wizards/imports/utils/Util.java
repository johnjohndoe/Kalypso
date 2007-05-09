package org.kalypso.ui.wizards.imports.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.base.ISzenarioSourceProvider;
import de.renew.workflow.cases.ICaseDataProvider;

/**
 * Holds utility methods
 * 
 * @author Patrice Congo
 */
public class Util
{
  public static final List<String> getAllName( Feature feature )
  {
    Object obj = feature.getProperty( KalypsoModelRoughnessConsts.GML_PROP_NAME );
    if( obj instanceof String )
    {
      return Arrays.asList( new String[] { (String) obj } );
    }
    else if( obj instanceof List )
    {

      ArrayList<String> names = new ArrayList<String>( (List<String>) obj );
      return names;
    }
    else
    {
      return null;
    }
  }

  /**
   * @param feature --
   *          the feature which workspace of to search for other feature of the same type having the specified uri
   * @param uri
   *          the uri to lookup
   * @return true is in the feature workspace there is a feature of the some type with the given
   */
  public static final boolean isInFeatureWorkspace( Feature feature, QName propQName, String uri )
  {

    return false;
  }

  /**
   * Gets the szenario data provider
   */
  public static final ICaseDataProvider getCaseDataProvider( )
  {
    try
    {
      IWorkbench workbench = PlatformUI.getWorkbench();
      IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      IEvaluationContext currentState = service.getCurrentState();
      ICaseDataProvider caseDataProvider = (ICaseDataProvider) currentState.getVariable( ISzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
      return caseDataProvider;
    }
    catch( Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }
}
