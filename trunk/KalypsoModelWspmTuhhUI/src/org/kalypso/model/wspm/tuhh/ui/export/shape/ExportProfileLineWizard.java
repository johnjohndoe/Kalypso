/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.ui.export.shape;

import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gml.ui.commands.exportshape.ExportShapeOperation;
import org.kalypso.gml.ui.commands.exportshape.ExportShapePage;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultFactory;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultInterpolationProfile;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSectionColumn;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard;
import org.kalypso.model.wspm.tuhh.ui.export.ProfileResultExportPage;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.shape.deegree.IShapeDataFactory;

public class ExportProfileLineWizard extends ExportProfilesWizard
{
  private final ExportShapePage m_exportShapePage;

  private final ProfileResultExportPage m_resultsPage;

  private final ExportProfileLineInterpolationPage m_interpolationPage;

  public ExportProfileLineWizard( final ProfileSelection selection, final String fileName )
  {
    super( selection );

    final IDialogSettings wizardSettings = PluginUtilities.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() );
    setDialogSettings( wizardSettings );

    final IWspmResultNode results = WspmResultFactory.createResultNode( null, selection.getContainer() );
    m_resultsPage = new ProfileResultExportPage( "profileResults", results ); //$NON-NLS-1$
    addPage( m_resultsPage );

    m_exportShapePage = new ExportShapePage( "exportShapePage", fileName ); //$NON-NLS-1$
    addPage( m_exportShapePage );

    m_interpolationPage = new ExportProfileLineInterpolationPage( "interpolationPage" );
    addPage( m_interpolationPage );

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard#exportProfiles(org.kalypso.model.wspm.core.gml.IProfileFeature[],
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected void exportProfiles( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws CoreException
  {
    final Charset shapeCharset = m_exportShapePage.getCharset();
    final String coordinateSystem = m_exportShapePage.getCoordinateSystem();
    final String shapeFileBase = m_exportShapePage.getShapeFileBase();
    final boolean doWritePrj = m_exportShapePage.isWritePrj();

    final IWspmResult[] results = m_resultsPage.getSelectedResults();

    final IProfileFeature[] interpolatedProfiles = interpolateProfiles( profiles, results );
    final WspmResultLengthSectionColumn[] lsColumns = m_resultsPage.getSelectedColumns();

    final IShapeDataFactory shapeDataFactory = new ProfileLineDataFactory( interpolatedProfiles, shapeCharset, coordinateSystem, lsColumns );

    try
    {
      final ICoreRunnableWithProgress operation = new ExportShapeOperation( shapeFileBase, shapeDataFactory, doWritePrj );
      operation.execute( monitor );
    }
    catch( final InvocationTargetException e )
    {
      final String msg = "Failed to export profiles";
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), msg, e.getTargetException() );
      throw new CoreException( status );
    }
    catch( final InterruptedException e )
    {
      throw new CoreException( Status.CANCEL_STATUS );
    }
  }

  private IProfileFeature[] interpolateProfiles( final IProfileFeature[] profiles, final IWspmResult[] results )
  {
    final Collection<IProfileFeature> allProfiles = new ArrayList<IProfileFeature>( (int) (profiles.length * 1.1) );

    allProfiles.addAll( Arrays.asList( profiles ) );

    for( final IWspmResult result : results )
    {
      final IProfileFeature[] interpolatedProfiles = createInterpolatedProfiles( result );
      allProfiles.addAll( Arrays.asList( interpolatedProfiles ) );
    }

    return allProfiles.toArray( new IProfileFeature[allProfiles.size()] );
  }

  private IProfileFeature[] createInterpolatedProfiles( final IWspmResult result )
  {
    final boolean interpolateForland = m_interpolationPage.shouldInterpolateForland();
    final boolean addInterpolatedProfiles = m_interpolationPage.shouldAddInterpolatedProfiles();
    if( !addInterpolatedProfiles )
      return new IProfileFeature[0];

    final Collection<IProfileFeature> interpolatedProfiles = new ArrayList<IProfileFeature>();

    final TuhhCalculation calculation = result.getCalculation();
    final TuhhReach reach = calculation.getReach();
    final WspmResultLengthSection lengthSection = result.getLengthSection();

    final WspmResultInterpolationProfile[] interpolationProfiles = lengthSection.findInterpolationStations();
    for( final WspmResultInterpolationProfile interpolationProfile : interpolationProfiles )
    {
      final IProfileFeature interpolatedProfile = interpolationProfile.createInterpolatedProfile( reach, !interpolateForland );
      interpolatedProfiles.add( interpolatedProfile );
    }

    return interpolatedProfiles.toArray( new IProfileFeature[interpolatedProfiles.size()] );
  }
}
