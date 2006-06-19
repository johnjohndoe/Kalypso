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
public class DynamicContentProvider implements IIntroXHTMLContentProvider
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
    // final String urlFromConfig = "http://localhost:8080/webdav/dss/intro/intropage.html";
    // final String urlFromConfig = "http://134.28.87.41:8080/webdav/dss/intro/intropage.html";
    // final File file = new File( "C:\\Tomcat 5.0\\webapps\\webdav\\dss\\intro\\intropage.xhtml" );
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
    // final String[] commandLineArgs = Platform.getCommandLineArgs();

    // final File file = new File( "C:/eclipse3.1_workspace/KalypsoFlowsData/dss/intro/intropage.html" );
    // String urlFromConfig = null;
    // try
    // {
    // urlFromConfig = file.toURL().toString();
    // }
    // catch( MalformedURLException e1 )
    // {
    // // TODO Auto-generated catch block
    // e1.printStackTrace();
    // }
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
          // final URL url = new URL( "http://org.eclipse.ui.intro/runAction?goto=" + urlFromConfig +"&");
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
