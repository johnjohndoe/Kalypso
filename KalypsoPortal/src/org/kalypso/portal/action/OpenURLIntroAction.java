package org.kalypso.portal.action;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.ui.intro.IIntroSite;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.KalypsoWorkFlowPlugin;

public class OpenURLIntroAction extends AbstractIntroAction

{

  // same as openURL, but resolves relative links
  public final static String PARAM_GOTO = "openURL";

  @Override
  public void run( IIntroSite site, Properties params )
  {
    super.run( site, params );
    final WorkflowContext wfContext = KalypsoWorkFlowPlugin.getDefault().getDefaultWorkflowContext();
    final String gotoURLAsString = (String) params.get( PARAM_GOTO );
    if( gotoURLAsString != null && gotoURLAsString.length() > 1 )
    {
      try
      {
        final URL gotoURL = wfContext.resolveURL( gotoURLAsString );
        getBrowser().setUrl( gotoURL.toString() );
      }
      catch( MalformedURLException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      return;
    }
  }

}
