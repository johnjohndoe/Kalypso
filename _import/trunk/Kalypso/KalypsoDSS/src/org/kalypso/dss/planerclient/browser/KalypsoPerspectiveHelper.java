package org.kalypso.dss.planerclient.browser;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.tools.ant.filters.ReplaceTokens;
import org.apache.tools.ant.filters.ReplaceTokens.Token;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.XMLMemento;
import org.eclipse.ui.internal.IWorkbenchConstants;
import org.eclipse.ui.internal.WorkbenchPage;
import org.eclipse.ui.internal.WorkbenchWindow;
import org.eclipse.ui.internal.misc.StatusUtil;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.commons.java.net.UrlResolverSingleton;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.ui.MementoUtils;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.IUrlResolver2;

public class KalypsoPerspectiveHelper
{

  /**
   * Diese Methode wechselt zur nächsten Perspective
   */
  public static boolean nextPerspective( String urlWithPageState, String perspectiveID, IWorkbenchPage activePage )
  {

    final IWorkbench workbench = PlatformUI.getWorkbench();
    WorkbenchPage page = null;
    final IPerspectiveRegistry registry = workbench.getPerspectiveRegistry();
    try
    {
      final URL url = new URL( urlWithPageState );
//      IProject project = ResourceUtilities.findProjectFromURL( url );
//      URL projectURL = project.getFullPath().toFile().toURL();
//      final IUrlResolver urlResolver = new UrlResolver();
//
//      // urlResolver.addReplaceToken( "project", "platform:/resource/" + project.getName() + "/" );
//      urlResolver.addReplaceToken( "project", projectURL.toExternalForm() + "/" );
//      // urlResolver.addReplaceToken( "calcdir", "platform:/resource/" + calcdir.getFullPath().toString() + "/" );
//
//      BufferedReader reader = new BufferedReader( new InputStreamReader( url.openStream() ) );
//      // Replace tokens
//      final ReplaceTokens rt = new ReplaceTokens( reader );
//      rt.setBeginToken( ':' );
//      rt.setEndToken( ':' );
//      for( final Iterator tokenIt = urlResolver.getReplaceEntries(); tokenIt.hasNext(); )
//      {
//        final Map.Entry entry = (Entry) tokenIt.next();
//
//        final Token token = new ReplaceTokens.Token();
//        token.setKey( (String) entry.getKey() );
//        token.setValue( (String) entry.getValue() );
//
//        rt.addConfiguredToken( token );
//      }

      // return createGMLWorkspace( new InputSource( rt ), gmlURL, urlResolver );

      // XMLMemento memento = XMLMemento.createReadRoot(new FileReader(
      // urlWithPageState));
      final InputStreamReader reader = new InputStreamReader( url.openStream() );
      final IMemento memento = MementoUtils.createMementoWithUrlResolver( XMLMemento.createReadRoot( reader ), new IUrlResolver2()
      {

        public URL resolveURL( String relative ) throws MalformedURLException
        {
          return UrlResolverSingleton.resolveUrl( url, relative );
        }

      } );
      final IMemento windowMemento = memento.getChild( IWorkbenchConstants.TAG_WINDOW );
      final IMemento pageMemento = windowMemento.getChild( IWorkbenchConstants.TAG_PAGE );

      // TODO this might be usefull if the implemenation changes
      // IMemento perspspectiveMemento = pageMemento
      // .getChild(IWorkbenchConstants.TAG_PERSPECTIVES);
      // IMemento singlePerspective = perspspectiveMemento
      // .getChild(IWorkbenchConstants.TAG_PERSPECTIVE);
      // IMemento descMemento = singlePerspective
      // .getChild(IWorkbenchConstants.TAG_DESCRIPTOR);
      // String perspectiveID = descMemento
      // .getString(IWorkbenchConstants.TAG_ID);

      IPerspectiveDescriptor realDesc = registry.findPerspectiveWithId( perspectiveID );
      IPerspectiveDescriptor oldPerspectiveDesc = activePage.getPerspective();
      activePage.closePerspective( oldPerspectiveDesc, true, false );
      // IWorkbenchWindow activWorkbenchWindow = workbench
      // .getActiveWorkbenchWindow();
      // page = (WorkbenchPage) activWorkbenchWindow.openPage(realDesc
      // .getId(), null);

      // Wieso diese Reihenfolge?? aber funktioniert
      // page.restoreState(pageMemento, realDesc);
      // page.setPerspective(realDesc);
      // page.resetPerspective();
      ((WorkbenchPage) activePage).restoreState( pageMemento, null );
      activePage.setPerspective( realDesc );
      activePage.resetPerspective();

    }
    catch( IOException e )
    {
      e.printStackTrace();
      handleException( e, page );
      return false;
    }
    catch( WorkbenchException e )
    {
      e.printStackTrace();
      handleException( e, page );
      return false;
    }

    return true;
  }

  /**
   * Handles workbench exception
   */
  private static void handleException( Exception e, WorkbenchPage page )
  {
    ErrorDialog.openError( page.getActivePart().getSite().getShell(), "Link Error", e.getMessage(), StatusUtil.newStatus( IStatus.WARNING, e.getMessage(), e ) );
  }

  /**
   * Diese Methode wechselt die Perspective. Problem: bevor die nächste Perspective eingeblendet wird, verkleinert sich
   * das Fenster um dann gleich wieder gross zu werden(unruhig für den Benutzer)
   */
  public static void nextPerspective( String url, String perspectiveID ) throws WorkbenchException, FileNotFoundException
  {

    // read Memento
    XMLMemento memento = XMLMemento.createReadRoot( new FileReader( url ) );
    IMemento windowMemento = memento.getChild( IWorkbenchConstants.TAG_WINDOW );

    IWorkbench workbench = PlatformUI.getWorkbench();
    IPerspectiveRegistry reg = workbench.getPerspectiveRegistry();
    IPerspectiveDescriptor persDesc = reg.findPerspectiveWithId( perspectiveID );
    if( persDesc == null )
      persDesc = reg.findPerspectiveWithId( reg.getDefaultPerspective() );
    WorkbenchWindow window = (WorkbenchWindow) workbench.getActiveWorkbenchWindow();
    IWorkbenchPage activePage = window.getActivePage();
    IPerspectiveDescriptor oldDesc = activePage.getPerspective();
    activePage.closePerspective( oldDesc, true, false );
    window.restoreState( windowMemento, persDesc );
  }
}
