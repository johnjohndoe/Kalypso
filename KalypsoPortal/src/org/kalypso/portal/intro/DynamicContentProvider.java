package org.kalypso.portal.intro;

import java.io.File;
import java.io.PrintWriter;
import java.net.URL;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.intro.config.IIntroContentProviderSite;
import org.eclipse.ui.intro.config.IIntroXHTMLContentProvider;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

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

    final File file = new File( "C:\\Tomcat 5.0\\webapps\\webdav\\dss\\intro\\intropage.xhtml" );
    URL url = null;
    try
    {

      url = file.toURL();
    }
    catch( Exception e )
    {
      e.printStackTrace();
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
      final Element frameSetE = domOwner.createElement( "frameset" );
      parent.appendChild( frameSetE );
      final Element frameE = domOwner.createElement( "frame" );
      frameSetE.appendChild( frameE );
      frameE.setAttribute( "src", url.toString() );
    }
  }

  public void dispose( )
  {

  }

}
