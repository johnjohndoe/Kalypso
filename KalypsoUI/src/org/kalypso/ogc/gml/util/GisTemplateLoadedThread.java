package org.kalypso.ogc.gml.util;

import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * <p>Dieser Thread wartet solange, bis eine Karte vollständig geladen wurde.</p>
 * <p>Danach macht er etwas (d.h. führt ein übergebenen Runnable aus) und beendet sich.</p>
 * 
 * @author belger
 */
public class GisTemplateLoadedThread extends Thread
{
  private final IMapModell m_modell;

  public GisTemplateLoadedThread( final IMapModell modell, final Runnable runnable )
  {
    super( runnable );
    
    m_modell = modell;
  }
  
  /**
   * @see java.lang.Thread#run()
   */
  public void run()
  {
    while( true )
    {
      if( isLoaded() )
        break;
      
      try
      {
        sleep( 500 );
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();
      }
    }
    
    super.run();
  }

  private boolean isLoaded()
  {
    final IKalypsoTheme[] themes = m_modell.getAllThemes();
    for( int i = 0; i < themes.length; i++ )
    {
      final IKalypsoTheme theme = themes[i];
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final CommandableWorkspace workspace = ((IKalypsoFeatureTheme)theme).getWorkspace();
        if( workspace == null )
          return false;
      }
    }

    // falls alle Workspace aller Feature-Themes geladen sind
    // gehen wir davon aus, dass die Karte geladen ist
    return true;
  }

}
