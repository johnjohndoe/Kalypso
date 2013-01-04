/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
package org.kalypso.ui.wizards.lengthsection;

import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.httpclient.URIException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;

/**
 * @author Gernot Belger
 */
class ShowLengthSectionOperation implements ICoreRunnableWithProgress
{
  private final IResultMeta[] m_results;

  final IWorkbenchWindow m_window;

  public ShowLengthSectionOperation( final IResultMeta[] results, final IWorkbenchWindow window )
  {
    m_results = results;
    m_window = window;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.7" ), m_results.length ); //$NON-NLS-1$

    final IStatusCollector log = new StatusCollector( Kalypso1d2dProjectPlugin.PLUGIN_ID );

    final Featuretemplate template = loadTemplate();

    int lsCount = 0;

    for( final IResultMeta resultMeta : m_results )
    {
      if( resultMeta instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta docResult = (IDocumentResultMeta)resultMeta;
        if( docResult.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.lengthSection )
        {
          try
          {
            final IStatus status = openLengthSection( docResult, template );
            log.add( status );
          }
          catch( final CoreException e )
          {
            log.add( e.getStatus() );
          }

          lsCount++;
        }
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    if( lsCount == 0 )
    {
      final String message = Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.9" );
      return new Status( IStatus.INFO, Kalypso1d2dProjectPlugin.PLUGIN_ID, message );
    }

    return log.asMultiStatusOrOK( "Problem(s) when trying to open longitudinal section(s)." );
  }

  private Featuretemplate loadTemplate( ) throws CoreException
  {
    /* load template from resource */
    final URL url = getClass().getResource( "resources/lengthsection.gft" ); //$NON-NLS-1$

    return GisTemplateHelper.loadGisFeatureTemplate( url, new NullProgressMonitor() );
  }

  private IStatus openLengthSection( final IDocumentResultMeta docResult, final Featuretemplate template ) throws CoreException
  {
    final String href = findLocation( docResult );

    final IWorkbenchPage page = m_window.getActivePage();

    final UIJob job = new UIJob( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ShowResultLengthSectionViewHandler.1" ) ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        try
        {
          final String secondaryId = href.replace( ':', '_' );

          final FeatureTemplateView featureView = (FeatureTemplateView)page.showView( FeatureTemplateView.ID, secondaryId, IWorkbenchPage.VIEW_ACTIVATE );

          final String linkType = "gml"; //$NON-NLS-1$
          // root feature
          final String featurePath = ""; //$NON-NLS-1$

          // set template to view in ui thread, sepcify href, featurePath and linkType
          featureView.setTemplate( template, null, featurePath, href, linkType );

          return Status.OK_STATUS;
        }
        catch( final Throwable e )
        {
          final String message = Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ShowResultLengthSectionViewHandler.0" ); //$NON-NLS-1$
          return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, message, e );
        }
      }
    };
    job.schedule();

    return Status.OK_STATUS;
  }

  private String findLocation( final IDocumentResultMeta docResult ) throws CoreException
  {
    try
    {
      final IPath fullPath = docResult.getFullPath();

      final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();
      final URL urlContext = ResourceUtilities.createURL( scenarioFolder );

      // path to gml, relative to context
      final String href = fullPath.toPortableString();

      final URL resolvedUrl = UrlResolverSingleton.resolveUrl( urlContext, href );

      return resolvedUrl.toExternalForm();
    }
    catch( URIException | MalformedURLException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, "Failed to resolve result location", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }
}