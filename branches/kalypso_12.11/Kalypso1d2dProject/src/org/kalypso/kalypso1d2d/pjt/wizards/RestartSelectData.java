/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypso1d2d.pjt.wizards;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.SelectResultData;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenario;

/**
 * @author Gernot Belger
 */
class RestartSelectData extends SelectResultData
{
  public static final String PROPERTY_SELECTED_RESTART = "selectedRestart"; //$NON-NLS-1$

  private final IObservableList m_restartResults = new WritableList( new ArrayList<>(), RestartElement.class );

  private RestartElement m_selectedRestart;

  private final IFolder m_currentScenario;

  public RestartSelectData( final IFolder currentScenario, final IScenarioResultMeta currentScenarioResult, final RestartElement[] restartResults )
  {
    super( currentScenarioResult );

    m_currentScenario = currentScenario;

    m_restartResults.addAll( Arrays.asList( restartResults ) );
  }

  public IObservableList getRestartResultSet( )
  {
    return m_restartResults;
  }

  public RestartElement[] getRestartResults( )
  {
    return (RestartElement[])m_restartResults.toArray( new RestartElement[m_restartResults.size()] );
  }

  public RestartElement getSelectedRestart( )
  {
    return m_selectedRestart;
  }

  public void setSelectedRestart( final RestartElement selectedRestart )
  {
    final RestartElement oldValue = m_selectedRestart;

    m_selectedRestart = selectedRestart;

    firePropertyChange( PROPERTY_SELECTED_RESTART, oldValue, selectedRestart );
  }

  public IFolder getCurrentScenario( )
  {
    return m_currentScenario;
  }

  public void initalizeRestartElements( final ITreeContentProvider contentProvider )
  {
    for( final Object object : m_restartResults )
    {
      final RestartElement element = (RestartElement)object;
      final IFile nodeFile = element.getNodeDocumentFile();

      final IDocumentResultMeta nodeResult = findNodeForRestartElement( contentProvider, nodeFile );
      element.setNodeResult( nodeResult );
    }
  }

  private IDocumentResultMeta findNodeForRestartElement( final ITreeContentProvider contentProvider, final IFile nodeFile )
  {
    if( nodeFile == null )
      return null;

    if( !nodeFile.exists() )
      return null;

    /* iterate through content provider for the right element */
    final Object[] rootElements = contentProvider.getElements( ResourcesPlugin.getWorkspace().getRoot() );
    for( final Object rootElement : rootElements )
    {
      final IDocumentResultMeta nodeResult = findNodeForRestartElement( contentProvider, nodeFile, rootElement );
      if( nodeResult != null )
        return nodeResult;
    }

    return null;
  }

  private IDocumentResultMeta findNodeForRestartElement( final ITreeContentProvider contentProvider, final IFile nodeFile, final Object treeElement )
  {
    if( treeElement instanceof IDocumentResultMeta )
    {
      final IDocumentResultMeta documentResult = (IDocumentResultMeta)treeElement;
      final DOCUMENTTYPE documentType = documentResult.getDocumentType();
      if( documentType == DOCUMENTTYPE.nodes )
      {
        final IFile docFile = resolveDocumentFile( documentResult );
        if( docFile != null && docFile.equals( nodeFile ) )
          return documentResult;
      }
    }
    else if( treeElement instanceof IResource )
    {
      /* break recursion if element cannot be part of this branch, as loading the real scenario file is costly */
      final IPath nodePath = nodeFile.getFullPath();
      final IPath elementPath = ((IResource)treeElement).getFullPath();
      if( !elementPath.isPrefixOf( nodePath ) )
        return null;
    }
    else if( treeElement instanceof IScenario )
    {
      /* break recursion if element cannot be part of this branch, as loading the real scenario file is costly */
      final IPath nodePath = nodeFile.getFullPath();
      final IFolder elementFolder = ((IScenario)treeElement).getFolder();
      final IPath elementPath = elementFolder.getFullPath();
      if( !elementPath.isPrefixOf( nodePath ) )
        return null;
    }

    /* recursion */
    final Object[] children = contentProvider.getChildren( treeElement );
    for( final Object child : children )
    {
      final IDocumentResultMeta nodeResult = findNodeForRestartElement( contentProvider, nodeFile, child );
      if( nodeResult != null )
        return nodeResult;
    }

    return null;
  }

  public IFile resolveDocumentFile( final IDocumentResultMeta documentResult )
  {
    try
    {
      final String restartPath = ResultMeta1d2dHelper.buildFullLocation( documentResult, m_currentScenario );
      final URL currentLocation = ResourceUtilities.createQuietURL( m_currentScenario );
      final URL docLocation = UrlResolverSingleton.resolveUrl( currentLocation, restartPath );
      return ResourceUtilities.findFileFromURL( docLocation );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public RestartElement createRestartElement( final IStepResultMeta stepResult )
  {
    final IDocumentResultMeta nodeResult = findNodeResult( stepResult );
    final IPath nodeResultPath = nodeResult.getFullPath();
    final IFile nodeResultFile = resolveDocumentFile( nodeResult );

    final RestartElement newElement = new RestartElement( nodeResultPath, nodeResultFile );
    newElement.setNodeResult( nodeResult );
    return newElement;
  }

  private IDocumentResultMeta findNodeResult( final IStepResultMeta stepResult )
  {
    /* find corresponding document element */
    final IFeatureBindingCollection<IResultMeta> children = stepResult.getChildren();
    for( final IResultMeta resultMeta : children )
    {
      if( resultMeta instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta docResult = (IDocumentResultMeta)resultMeta;
        if( docResult.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.nodes )
          return docResult;
      }
    }

    return null;
  }

  public RestartElement findRestartElement( final IStepResultMeta stepResult )
  {
    for( final Object object : m_restartResults )
    {
      final RestartElement element = (RestartElement)object;
      final IStepResultMeta elementStep = element.getStepResult();

      if( stepResult == elementStep )
        return element;
    }

    return null;
  }

  public ShowType findShowType( final RestartElement[] restartResults )
  {
    ShowType maxType = ShowType.current;

    for( final RestartElement restartElement : restartResults )
    {
      final ShowType elementType = findShowType( restartElement );
      if( maxType.compareTo( elementType ) < 0 )
        maxType = elementType;
    }

    return maxType;
  }

  public ShowType findShowType( final RestartElement restartElement )
  {
    final IFolder currentScenario = getCurrentScenario();

    final IStepResultMeta stepResult = restartElement.getStepResult();
    if( stepResult == null )
      return ShowType.current;

    final Pair<IProject, IFolder> externalLocation = ResultMeta1d2dHelper.determineExternalLocation( stepResult, currentScenario );
    if( externalLocation.getLeft() != null )
      return ShowType.all;
    if( externalLocation.getRight() != null )
      return ShowType.project;

    return ShowType.current;
  }
}