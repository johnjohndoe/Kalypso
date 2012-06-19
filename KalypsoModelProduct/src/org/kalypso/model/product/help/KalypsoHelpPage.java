package org.kalypso.model.product.help;

import org.eclipse.help.base.AbstractHelpDisplay;

public class KalypsoHelpPage extends AbstractHelpDisplay
{

  public KalypsoHelpPage( )
  {
  }

  @Override
  public String getHelpHome( final String hostname, final int port, final String tab )
  {
    return "https://sourceforge.net/apps/mediawiki/kalypso/index.php?title=Main_Page"; //$NON-NLS-1$
  }

  /**
   * method can be triggered from:
   * 
   * <pre>
   * final IWorkbenchHelpSystem helpSystem = PlatformUI.getWorkbench().getHelpSystem();
   * helpSystem.displayHelpResource( &quot;main&quot; );
   * </pre>
   */
  @Override
  public String getHelpForTopic( final String topic, final String hostname, final int port )
  {
    if( "main".equals( topic ) ) //$NON-NLS-1$
      return "https://sourceforge.net/apps/mediawiki/kalypso/index.php?title=Main_Page"; //$NON-NLS-1$

    return null;
  }

}
