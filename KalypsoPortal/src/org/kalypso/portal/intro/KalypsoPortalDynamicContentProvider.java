package org.kalypso.portal.intro;

import java.io.PrintWriter;
import java.net.URL;

import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.intro.config.IIntroContentProviderSite;
import org.eclipse.ui.intro.config.IIntroXHTMLContentProvider;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

/**
 * dynamic content provider that provides a html frame to add an external pageURL.
 * 
 * @author kuepfer
 */
public class KalypsoPortalDynamicContentProvider implements IIntroXHTMLContentProvider
{

  public void init( IIntroContentProviderSite site )
  {
  }

  public void createContent( String id, PrintWriter out )
  {
    // do nothing (no emmbedded HTML-Widget used)
  }

  public void createContent( String id, Composite parent, FormToolkit toolkit )
  {
    // do nothing (no FormToolKit in use)
  }

  public void createContent( String id, Element parent )
  {
    final Document domOwner = parent.getOwnerDocument();
    // get the first welcome page from the specified URL (the argument is included in the *.ini file)
    // entry must be after the eclipse args-list: 
    // eg: -PortalURL http://localhost:8080/directory/firstpage.html 
    final String portalURLArgument = "-portalurl";
    String urlFromConfig = null;
    final String[] applicationArgs = Platform.getApplicationArgs();
    for( int i = 0; i < applicationArgs.length; i++ )
    {
      if( portalURLArgument.equalsIgnoreCase( applicationArgs[i] ) && i + 1 < applicationArgs.length )
      {
        urlFromConfig = applicationArgs[i + 1];
        break;
      }
    }
    if( "headID".equals( id ) )
    {
      final Comment comment = domOwner.createComment( "headID" );
      parent.appendChild( comment );
      // generate head
      // add styles here
    }
    if( "frameID".equals( id ) )
    {
      try
      {
        if( urlFromConfig == null || urlFromConfig.length() < 1 )
        {
          final Text warning = domOwner.createTextNode( "Es wurde keine Startseite an das Programm uebergeben."// 
              + " Erforderlicher Parameter: PortalURL,"// 
              + " Beispiel: -PortalURL http://www.kalypso.wb.tu-harburg.de ,"// 
              + " (als Argument oder in *.ini Datei uebergeben.)" );
          parent.appendChild( warning );

        }
        else
        {

          // important: must allways follow openURL-introActions to stay in securityContext for excecuting
          // runAction-introActions
          final URL url = new URL( "http://org.eclipse.ui.intro/openURL?url=" + urlFromConfig );
          // tricky: generate frame, otherwise relative urls can not be resolved !
          final Element frameSetE = domOwner.createElement( "frameset" );
          parent.appendChild( frameSetE );
          final Element frameE = domOwner.createElement( "frame" );
          frameSetE.appendChild( frameE );
          frameE.setAttribute( "src", url.toString() );
        }

      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  public void dispose( )
  {
    // nothing to dispose
  }

}
