/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ui.wizards.results;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.project.Scenario1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * Holds the data (except the GisTemplateMapModell), that is needed for the addThemeCommand.
 * 
 * @author Thomas Jung
 */
public class ResultAddLayerCommandData
{
  public static final String PROPERTY_RESULT_TYPE = "1D2DResultType"; //$NON-NLS-1$

  public static final String PROPERTY_RESULT_NODE_PARAMETER_TYPE = "1D2DResultNodeParameterType"; //$NON-NLS-1$

  /** the gml-id of the document result */
  private static final String PROPERTY_RESULT_DOCUMENT_ID = "1D2DResultDocumentID"; //$NON-NLS-1$

  /** <code>true</code>, if it is an external result theme */
  private static final String PROPERTY_RESULT_EXTERNAL = "1D2DResultIsExternal"; //$NON-NLS-1$

  private final Map<String, String> m_properties = new HashMap<>();

  private final String m_resultType = "gml"; //$NON-NLS-1$

  private final String m_featurePath;

  private final String m_style;

  private String m_styleLocation;

  private IFile m_sldFile = null;

  private boolean m_selected;

  private final IFolder m_scenarioFolder;

  private final String m_type;

  private final IDocumentResultMeta m_documentResult;

  public ResultAddLayerCommandData( final String featurePath, final String style, final String styleLocation, final IFolder scenarioFolder, final String type, final IDocumentResultMeta documentResult )
  {
    m_featurePath = featurePath;
    m_style = style;
    m_styleLocation = styleLocation;
    m_scenarioFolder = scenarioFolder;
    m_type = type;
    m_documentResult = documentResult;

    updateStyleLocation();
  }

  public void setValues( final String styleLocation )
  {
    m_styleLocation = styleLocation;

    updateStyleLocation();
  }

  public void setSldFile( final IFile sldFile )
  {
    m_sldFile = sldFile;

    updateStyleLocation();
  }

  private void updateStyleLocation( )
  {
    final Scenario1D2D scenario = new Scenario1D2D( m_scenarioFolder );
    final IFolder resultsFolder = scenario.getResultsFolder();
    final String resFolder = resultsFolder.getFullPath().toPortableString();

    final IFolder stylesFolder = scenario.getStylesFolder();
    final String defaultPath = stylesFolder.getFullPath().toPortableString();
    final String relativePathTo = FileUtilities.getRelativePathTo( resFolder, defaultPath );

    if( m_sldFile == null )
      m_styleLocation = ""; //$NON-NLS-1$
    else
    {
      final String sldFileName = m_sldFile.getName();
      final IPath styleLocation = new Path( ".." ).append( relativePathTo ).append( m_type ).append( sldFileName ); //$NON-NLS-1$
      m_styleLocation = styleLocation.toPortableString();

      final Pair<String, String> types = determineTypes();
      final String nodeParameterType = types.getRight();
      if( nodeParameterType != null )
        setProperty( PROPERTY_RESULT_NODE_PARAMETER_TYPE, nodeParameterType );

      /* properties that identify the result theme */
      setProperty( PROPERTY_RESULT_DOCUMENT_ID, m_documentResult.getId() );

      final Pair<IProject, IFolder> externalLocation = determineExternalLocation();
      final boolean isExternal = externalLocation.getRight() != null;
      setProperty( PROPERTY_RESULT_EXTERNAL, Boolean.toString( isExternal ) );
    }
  }

  public String getThemeName( )
  {
    Assert.isNotNull( m_sldFile );

    final Pair<String, String> types = determineTypes();
    final Pair<IProject, IFolder> externalLocation = determineExternalLocation();

    final String type = types.getLeft();
    final String nodeParameterType = types.getRight();

    final IProject externalProject = externalLocation.getLeft();
    final IFolder externalScenario = externalLocation.getRight();

    if( NodeResultHelper.NODE_TYPE.equals( type ) )
    {
      final String nodeParameterTypeName = NodeResultHelper.translateNodeParameterType( nodeParameterType );
      return formatThemeName( nodeParameterTypeName, externalScenario, externalProject );
    }

    if( NodeResultHelper.LINE_TYPE.equals( type ) )
      return formatThemeName( ResultMeta1d2dHelper.STR_THEME_NAME_ISOLINE, externalScenario, externalProject );

    if( NodeResultHelper.POLYGON_TYPE.equals( type ) )
      return formatThemeName( ResultMeta1d2dHelper.STR_THEME_NAME_ISOAREA, externalScenario, externalProject );

    throw new IllegalStateException();
  }

  private Pair<String, String> determineTypes( )
  {
    final String type = m_properties.get( PROPERTY_RESULT_TYPE );

    Assert.isNotNull( m_sldFile );

    if( NodeResultHelper.NODE_TYPE.equals( type ) )
    {
      final String styleFilename = m_sldFile.getName();

      // FIXME: mega ugly
      final String typeNameFromSldFileName = ResultMeta1d2dHelper.resolveResultTypeFromSldFileName( styleFilename, type );
      return Pair.of( type, typeNameFromSldFileName );
    }

    return Pair.of( type, null );
  }

  public String getResultType( )
  {
    return m_resultType;
  }

  public String getFeaturePath( )
  {
    return m_featurePath;
  }

  public String getSource( )
  {
    return buildSourePath();
  }

  public String getStyle( )
  {
    return m_style;
  }

  public String getStyleLocation( )
  {
    return m_styleLocation;
  }

  public IFile getSldFile( )
  {
    return m_sldFile;
  }

  public boolean isSelected( )
  {
    return m_selected;
  }

  public void setSelected( final boolean selected )
  {
    m_selected = selected;
  }

  public void setProperty( final String name, final String value )
  {
    m_properties.put( name, value );
  }

  public Map<String, String> getProperties( )
  {
    return Collections.unmodifiableMap( m_properties );
  }

  private String formatThemeName( final String format, final IFolder externalScenario, final IProject externalProject )
  {
    /* get infos about calc unit */
    final IResultMeta timeStepMeta = m_documentResult.getOwner();
    final IResultMeta calcUnitMeta = timeStepMeta == null ? null : m_documentResult.getOwner().getOwner();

    final String documentName = m_documentResult.getName();

    /* build name parts */
    final Collection<String> components = new ArrayList<>( 5 );
    components.add( String.format( "%s (%s)", documentName, format ) );

    if( timeStepMeta != null )
      components.add( timeStepMeta.getName() );

    if( calcUnitMeta != null )
      components.add( calcUnitMeta.getName() );

    if( externalScenario != null )
      components.add( externalScenario.getName() );

    if( externalProject != null )
      components.add( externalProject.getName() );

    /* concatenate these parts */
    return StringUtils.join( components, ", " ); //$NON-NLS-1$
  }

  private String buildSourePath( )
  {
    final Pair<IProject, IFolder> externalLocation = determineExternalLocation();

    final IProject documentProject = externalLocation.getLeft();
    final IFolder documentScenarioFolder = externalLocation.getRight();

    final IPath documentPath = m_documentResult.getFullPath();

    if( documentScenarioFolder == null )
    {
      /* document is part of current scenario, make relative path to scenario map file */
      final IPath scenarioRelativePath = Path.fromPortableString( ".." ).append( documentPath );
      return scenarioRelativePath.toPortableString();
    }

    final IFile documentDataFile = documentScenarioFolder.getFile( documentPath );

    if( documentProject == null )
    {
      /* document is in the same project, but in another scenario: make project relative path */
      final IPath projectRelativePath = documentDataFile.getProjectRelativePath();
      return String.format( "%s/%s", UrlResolver.PROJECT_PROTOCOLL, projectRelativePath.toPortableString() ); //$NON-NLS-1$
    }

    /* outside of the current project: make absolute path */
    return ResourceUtilities.createQuietURL( documentDataFile ).toExternalForm();
  }

  /**
   * Determine, if the document is an external result or not.<br/>
   * If it is, the components of the return value are set accordingly.
   * 
   * @return If the project is set, it is an result of an different project. If the (scenario-)folder is set, it is the result of a different scenario.
   */
  private Pair<IProject, IFolder> determineExternalLocation( )
  {
    final URL resultsLocation = m_documentResult.getWorkspace().getContext();
    final IFile resultsFile = ResourceUtilities.findFileFromURL( resultsLocation );
    final IFolder documentScenarioFolder = (IFolder)resultsFile.getParent().getParent();

    if( documentScenarioFolder.equals( m_scenarioFolder ) )
    {
      /* local result */
      return Pair.of( null, null );
    }

    final IProject documentProject = documentScenarioFolder.getProject();

    if( documentProject.equals( m_scenarioFolder.getProject() ) )
    {
      /* document is in the same project, but in another scenario */
      return Pair.of( null, documentScenarioFolder );
    }

    /* outside of the current project */
    return Pair.of( documentProject, documentScenarioFolder );
  }
}