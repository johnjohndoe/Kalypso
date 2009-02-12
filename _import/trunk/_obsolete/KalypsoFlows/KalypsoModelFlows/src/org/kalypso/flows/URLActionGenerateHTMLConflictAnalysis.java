/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.flows;

import java.io.FileOutputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.ide.ResourceUtil;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.browser.AbstractURLActionAnalizeTheme;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * @author kuepfer
 */
public class URLActionGenerateHTMLConflictAnalysis extends AbstractURLActionAnalizeTheme
{
  private static final String BGColor = "#FFFFFF";

  private final static String PARAM_WORKSPACE_RESOURCE = "tragetResource";

  private final static String PARAM_HTML_TITLE = "title";

  /**
   * @see org.kalypso.workflow.ui.browser.AbstractURLActionAnalizeTheme#analyze(org.kalypso.ogc.gml.IKalypsoTheme[])
   */
  @Override
  public boolean analyze( final String[] linkTypes, final IKalypsoTheme[] themes, final ICommandURL commandURL )
  {
    final IFile file = ResourceUtil.getFile( getActiveEditor().getEditorInput() );

    final String pathGMT = ResourceUtilities.createURLSpec( file.getFullPath() );

    final String title = commandURL.getParameter( PARAM_HTML_TITLE );
    String linkTypeString = "";
    if( linkTypes != null )
    {
      for( int i = 0; i < linkTypes.length; i++ )
      {
        if( i < 1 )
          linkTypeString += linkTypes[i] + DEFAULT_SEPARATOR;
        else
          linkTypeString += linkTypes[i];
      }
    }
    final StringBuffer result = new StringBuffer( "<html><body bgcolor=\"" + BGColor + "\">" );
    generateHTMLFragmentTitle( result, title );
    final ArrayList<IKalypsoFeatureTheme> allThemes = new ArrayList<IKalypsoFeatureTheme>();
    for( final IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme) theme;
        if( ArrayUtils.contains( linkTypes, kft.getType() ) )
        {
          generateHTMLFragmentTheme( result, pathGMT, kft.getLabel(), linkTypeString );
          allThemes.add( kft );
        }
      }
    }
    generateHTMLFragementAllThemes( result, allThemes.toArray( new IKalypsoFeatureTheme[allThemes.size()] ), linkTypeString, pathGMT );
    result.append( "</body></html>" );
    final String resource = commandURL.getParameter( PARAM_WORKSPACE_RESOURCE );
    try
    {
      writeHTMLFile( resource, result );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  private void generateHTMLFragementAllThemes( final StringBuffer result, final IKalypsoTheme[] allThemes, final String linkTypes, final String pathGMT )
  {
    String propList = "";
    for( int i = 0; i < allThemes.length; i++ )
    {
      if( i < allThemes.length - 1 )
        propList += allThemes[i].getName() + DEFAULT_SEPARATOR;
      else
        propList += allThemes[i].getName();
    }
    result.append( "<br><a href=\"kalypso://openFLOWSFilterDialog?" );
    if( propList != null )
      result.append( URLActionFLOWSAddFilterToGMT.PARAM_THEME_LIST + "=" + propList );
    result.append( "&" + URLActionFLOWSAddFilterToGMT.PARAM_PATH_GMT + "=" + pathGMT + "&" + URLActionFLOWSAddFilterToGMT.PARAM_GEOM_OPERATION + "=Intersects&" );
    if( linkTypes != null )
      result.append( PARAM_LIST_LINKTYPES + "=" + linkTypes );
    result.append( "\">Gleicher Filter für alle Themen setzen</a><br>" );

  }

  private void generateHTMLFragmentTheme( final StringBuffer result, final String pathGMT, final String themeTitle, final String linkTypes )
  {
    result.append( "<b><font face=\"verdana, arial, helvetica\" color=\"#00257E\" size=\"2\">" + themeTitle + "<br></font>" );
    result.append( "<a href=\"kalypso://openFLOWSFilterDialog?" );
    result.append( URLActionFLOWSAddFilterToGMT.PARAM_THEME_LIST + "=" + themeTitle );
    result.append( "&" + URLActionFLOWSAddFilterToGMT.PARAM_PATH_GMT + "=" + pathGMT + "&" + URLActionFLOWSAddFilterToGMT.PARAM_GEOM_OPERATION + "=Intersects&" );
    if( linkTypes != null )
      result.append( PARAM_LIST_LINKTYPES + "=" + linkTypes );
    result.append( "\">Filter anpassen</a><br><br>" );
  }

  private void generateHTMLFragmentTitle( final StringBuffer result, final String title )
  {
    result.append( " <table width=\"100%\"><tr><td align=\"left\">" );
    result.append( "<b><font face=\"verdana, arial, helvetica\" color=\"#00257E\" size=\"+1\">" + title + "<br></font></b>" );
    result.append( " </td><td align=\"right\">" );
    result.append( " </td></tr></table>" );
    result.append( "</br>" );

  }

  private void writeHTMLFile( final String resource, final StringBuffer result ) throws MalformedURLException
  {
    final WorkflowContext workFlowContext = getWorkFlowContext();
    final URL urlHtml = workFlowContext.resolveURL( resource );
    final IFile file = ResourceUtilities.findFileFromURL( urlHtml );
    final String content = result.toString();
    OutputStream htmlOutputStream = null;
    try
    {
      htmlOutputStream = new FileOutputStream( file.getLocation().toFile() );
      IOUtils.write( content, htmlOutputStream );
      file.getParent().refreshLocal( IFile.DEPTH_INFINITE, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      generateMessageDialog( "Error while writing file: " + urlHtml.toExternalForm(), IStatus.ERROR );
    }
    finally
    {
      IOUtils.closeQuietly( htmlOutputStream );
    }
  }
}
