package org.kalypso.ui.wizards.imports.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * Holds utility methods
 * 
 * @author Patrice Congo
 */
public class Util
{
  public static final List<String> getAllName( final Feature feature )
  {
    final Object obj = feature.getProperty( Feature.QN_NAME );
    if( obj instanceof String )
    {
      return Arrays.asList( new String[] { (String) obj } );
    }
    else if( obj instanceof List )
    {

      final ArrayList<String> names = new ArrayList<String>( (List<String>) obj );
      return names;
    }
    else
    {
      return null;
    }
  }

  /**
   * @param feature
   *          -- the feature which workspace of to search for other feature of the same type having the specified uri
   * @param uri
   *          the uri to lookup
   * @return true is in the feature workspace there is a feature of the some type with the given
   */
  public static final boolean isInFeatureWorkspace( final Feature feature, final QName propQName, final String uri )
  {

    return false;
  }

  /**
   * Gets the szenario data provider
   */
  public static final SzenarioDataProvider getCaseDataProvider( )
  {
    try
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();
      final SzenarioDataProvider caseDataProvider = (SzenarioDataProvider) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      return caseDataProvider;
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }
}
