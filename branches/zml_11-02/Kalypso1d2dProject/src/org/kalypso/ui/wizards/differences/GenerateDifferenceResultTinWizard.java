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
package org.kalypso.ui.wizards.differences;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.URL;

import javax.activation.UnsupportedDataTypeException;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.DifferenceResultTinHandler;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.IMathOperatorDelegate;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.IMathOperatorDelegate.MATH_OPERATOR;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.MinMaxCatcher;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypso.ui.wizards.results.Result1d2dMetaComparator;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypso.ui.wizards.results.ThemeConstructionFactory;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;
import org.kalypso.ui.wizards.results.filters.NonTinDocumentResultViewerFilter;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * Wizard to show length sections to the chart view.
 * 
 * @author Thomas Jung
 */
public class GenerateDifferenceResultTinWizard extends Wizard
{
  private final MinMaxCatcher m_minMaxCatcher = new MinMaxCatcher();

  private static final String PAGE_SELECT_DESTINATION_RESULTS_NAME = "selectDestinationResults"; //$NON-NLS-1$

  private static final String PAGE_SELECT_MASTER_RESULTS_NAME = "selectMasterResults"; //$NON-NLS-1$

  private static final String PAGE_SELECT_SLAVE_RESULTS_NAME = "selectSlaveResults"; //$NON-NLS-1$

  private final IScenarioResultMeta m_resultModel;

  private final IFolder m_scenarioFolder;

  private IFile m_selectedResultFile;

  public GenerateDifferenceResultTinWizard( final IFolder scenarioFolder, final IScenarioResultMeta resultModel )
  {
    m_scenarioFolder = scenarioFolder;
    m_resultModel = resultModel;
    setWindowTitle( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.3") ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {

    // select master document page
    final NonTinDocumentResultViewerFilter resultFilter = new NonTinDocumentResultViewerFilter();
    final Result1d2dMetaComparator comparator = new Result1d2dMetaComparator();

    final SelectResultWizardPage selectMasterResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_MASTER_RESULTS_NAME, Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.4"), null, resultFilter, comparator, null, null ); //$NON-NLS-1$
    final SelectResultWizardPage selectSlaveResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_SLAVE_RESULTS_NAME, Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.5"), null, resultFilter, comparator, null, null ); //$NON-NLS-1$

    final DocumentResultViewerFilter resultFilter2 = new DocumentResultViewerFilter();
    final ThemeConstructionFactory themeConstructionFactory = new ThemeConstructionFactory( m_scenarioFolder );
    final SelectResultWizardPage selectDestinationResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_DESTINATION_RESULTS_NAME, Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.6"), null, resultFilter2, comparator, themeConstructionFactory, null ); //$NON-NLS-1$

    selectMasterResultWizardPage.setResultMeta( m_resultModel );
    selectSlaveResultWizardPage.setResultMeta( m_resultModel );
    selectDestinationResultWizardPage.setResultMeta( m_resultModel );

    addPage( selectMasterResultWizardPage );
    addPage( selectSlaveResultWizardPage );
    addPage( selectDestinationResultWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {

    final MATH_OPERATOR operator = IMathOperatorDelegate.MATH_OPERATOR.eMinus;

    final GM_TriangulatedSurface[] surfaces = new GM_TriangulatedSurface[2];

    IDocumentResultMeta.DOCUMENTTYPE masterDocType = null;
    IDocumentResultMeta.DOCUMENTTYPE slaveDocType = null;

    /* check user input */
    // master
    final SelectResultWizardPage masterResultPage = (SelectResultWizardPage) getPage( PAGE_SELECT_MASTER_RESULTS_NAME );
    final IResultMeta[] masterResults = masterResultPage.getSelectedResults();

    if( masterResults.length == 0 )
    {
      MessageDialog.openInformation( getShell(), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.7"), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.8") ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }
    else
    {
      if( masterResults[0] instanceof IDocumentResultMeta )
      {
        masterDocType = ((IDocumentResultMeta) masterResults[0]).getDocumentType();
      }
    }

    // slave
    final SelectResultWizardPage slaveResultPage = (SelectResultWizardPage) getPage( PAGE_SELECT_SLAVE_RESULTS_NAME );
    final IResultMeta[] slaveResults = slaveResultPage.getSelectedResults();

    if( slaveResults.length == 0 )
    {
      MessageDialog.openInformation( getShell(), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.9"), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.10") ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }
    else
    {
      if( slaveResults[0] instanceof IDocumentResultMeta )
      {
        slaveDocType = ((IDocumentResultMeta) slaveResults[0]).getDocumentType();
      }
    }

    if( !slaveDocType.equals( masterDocType ) )
    {
      if( !MessageDialog.openQuestion( getShell(), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.11"), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.12") ) ) //$NON-NLS-1$ //$NON-NLS-2$
        return false;
    }

    final SelectResultWizardPage destinationResultPage = (SelectResultWizardPage) getPage( PAGE_SELECT_DESTINATION_RESULTS_NAME );
    final IResultMeta[] destinationResults = destinationResultPage.getSelectedResults();

    // destination
    if( destinationResults.length == 0 )
    {
      MessageDialog.openInformation( getShell(), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.13"), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.14") ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }
    else
    {
      IResultMeta destResult = null;

      // take the first selected step result
      for( IResultMeta resultMeta : destinationResults )
      {
        if( resultMeta instanceof IStepResultMeta )
        {
          destResult = resultMeta;
        }
      }

      if( destResult == null )
      {
        MessageDialog.openInformation( getShell(), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.15"), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.16") ); //$NON-NLS-1$ //$NON-NLS-2$
        return false;
      }
    }

    /* Start */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.17"), 100 ); //$NON-NLS-1$

        try
        {
          GM_TriangulatedSurface masterSurface = null;
          GM_TriangulatedSurface slaveSurface = null;

          monitor.subTask( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.18") ); //$NON-NLS-1$
          masterSurface = getSurfaceData( masterResults[0] );
          monitor.worked( 10 );
          if( masterSurface == null )
            return StatusUtilities.createErrorStatus( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.19") ); //$NON-NLS-1$

          monitor.subTask( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.20") ); //$NON-NLS-1$
          slaveSurface = getSurfaceData( slaveResults[0] );
          monitor.worked( 10 );
          if( slaveSurface == null )
            return StatusUtilities.createErrorStatus( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.21") ); //$NON-NLS-1$

          surfaces[0] = masterSurface;
          surfaces[1] = slaveSurface;

          IResultMeta destResult = null;

          // take the first selected step result
          for( IResultMeta resultMeta : destinationResults )
          {
            if( resultMeta instanceof IStepResultMeta )
            {
              destResult = resultMeta;
            }
          }

          if( destResult == null )
            return StatusUtilities.createErrorStatus( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.22") ); //$NON-NLS-1$

          monitor.subTask( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.23") ); //$NON-NLS-1$

          IPath docPath = destResult.getFullPath();
          IFolder folder = m_scenarioFolder.getFolder( docPath );

          /* generate unique name for difference file */
          String name = "tin"; //$NON-NLS-1$
          String extension = ".gml"; //$NON-NLS-1$
          File parentDir = docPath.toFile();

          // check, if file already exists and get the unique name */
          final File tinPath = new File( parentDir, "Tin" ); //$NON-NLS-1$
          String uniqueFileName = FileUtilities.createNewUniqueFileName( name, extension, tinPath );

          IFolder destPath = folder.getFolder( "Tin" ); //$NON-NLS-1$
          IFile destFile = destPath.getFile( uniqueFileName );

          IStatus status = DifferenceResultTinHandler.generateDifferences( surfaces, operator, destFile, m_minMaxCatcher, monitor );

          /* update resource folder */
          IContainer parent = destFile.getParent();
          parent.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );

          monitor.worked( 3 );

          /* update the result db */
          if( status.isOK() )
          {
            /* create the path entry for the document */
            final int extensionIndex = destFile.getName().lastIndexOf( "." ); //$NON-NLS-1$
            final String substring = destFile.getName().substring( 0, extensionIndex );

            /* create filename */
            final String param = "DIFFERENCE"; //$NON-NLS-1$
            final String paramName = substring + "_" + param + extension; //$NON-NLS-1$

            /* we "know", that the results are stored in the "Tin" folder */
            Path path = new Path( "Tin/" + paramName ); //$NON-NLS-1$

            // get min max via a minmaxCatcher during processing.
            BigDecimal min = m_minMaxCatcher.getMinValue();
            BigDecimal max = m_minMaxCatcher.getMaxValue();

            if( destResult instanceof IStepResultMeta )
            {
              // TODO: set a good description e.g.
              final String description = Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.32"); //$NON-NLS-1$

              IStepResultMeta stepResult = (IStepResultMeta) destResult;
              ResultMeta1d2dHelper.addDocument( stepResult, "Differenzen", description, IDocumentResultMeta.DOCUMENTTYPE.tinDifference, path, Status.OK_STATUS, min, max ); //$NON-NLS-1$
            }
            else
            {
              throw new UnsupportedDataTypeException( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.34") ); //$NON-NLS-1$
              // TODO: cleanFiles();
            }
          }
          else
            return status;

          monitor.worked( 1 );

        }
        catch( Exception e )
        {
          e.printStackTrace();
          // TODO: cleanFiles();
          return StatusUtilities.statusFromThrowable( e, Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.35") ); //$NON-NLS-1$
        }
        catch( final Throwable t )
        {
          // TODO: cleanFiles();
          throw new InvocationTargetException( t );
        }
        finally
        {
          monitor.done();
        }
        return StatusUtilities.createOkStatus( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.36") ); //$NON-NLS-1$

      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.37"), status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );

  }

  public IFile getSelection( )
  {
    return m_selectedResultFile;
  }

  private GM_TriangulatedSurface getSurfaceData( final IResultMeta resultMeta )
  {
    /* get the result data */
    if( resultMeta instanceof IDocumentResultMeta )
    {
      IDocumentResultMeta docResult = (IDocumentResultMeta) resultMeta;

      DOCUMENTTYPE documentType = docResult.getDocumentType();

      if( documentType == DOCUMENTTYPE.tinWsp || documentType == DOCUMENTTYPE.tinDepth || documentType == DOCUMENTTYPE.tinVelo || documentType == DOCUMENTTYPE.tinShearStress
          || documentType == DOCUMENTTYPE.tinTerrain )
      {
        try
        {
          IPath docPath = docResult.getFullPath();
          if( docPath == null )
            return null;

          final URL scenarioURL = ResourceUtilities.createURL( m_scenarioFolder );
          final URL surfaceURL = UrlUtilities.resolveWithZip( scenarioURL, docPath.toPortableString() );

          final GMLWorkspace w = GmlSerializer.createGMLWorkspace( surfaceURL, null );

          final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

          w.accept( new TransformVisitor( targetCRS ), w.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

          GM_Object geometryProperty = w.getRootFeature().getDefaultGeometryProperty();

          if( geometryProperty instanceof GM_TriangulatedSurface )
          {
            return (GM_TriangulatedSurface) geometryProperty;
          }
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
        return null;
      }
    }
    return null;
  }

}
