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
package org.kalypso.model.km.internal.ui.kmupdate;

import java.io.File;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.KMParameter;
import org.kalypso.model.km.internal.KMPlugin;
import org.kalypso.model.km.internal.core.IKMValue;
import org.kalypso.model.km.internal.core.ProfileDataSet;
import org.kalypso.model.km.internal.core.ProfileFactory;
import org.kalypso.model.km.internal.i18n.Messages;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author Gernot Belger
 */
public class KMUpdateOperation implements ICoreRunnableWithProgress
{
  private final List<FeatureChange> m_featureChanges = new ArrayList<FeatureChange>();

  private final Map<KMChannel, KalininMiljukovType> m_channels;

  private final CommandableWorkspace m_workspace;

  public KMUpdateOperation( final CommandableWorkspace workspace, final Map<KMChannel, KalininMiljukovType> channels )
  {
    m_workspace = workspace;
    m_channels = channels;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.21" ), m_channels.size() + 10 ); //$NON-NLS-1$

    final IStatus calculationStatus = calculateKM( monitor );
    if( !calculationStatus.isOK() )
      return calculationStatus;

    final FeatureChange[] change = m_featureChanges.toArray( new FeatureChange[m_featureChanges.size()] );
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( m_workspace, change );
    try
    {
      monitor.subTask( "Changing model..." );
      m_workspace.postCommand( command );
      ProgressUtilities.worked( monitor, 10 );
    }
    catch( final Exception e )
    {
      final IStatus error = new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.26" ), e ); //$NON-NLS-1$
      KMPlugin.getDefault().getLog().log( error );
      return error;
    }

    return calculationStatus;
  }

  private IStatus calculateKM( final IProgressMonitor monitor ) throws CoreException
  {
    final List<IStatus> problems = new ArrayList<IStatus>();

    for( final Entry<KMChannel, KalininMiljukovType> entry : m_channels.entrySet() )
    {
      final KMChannel channel = entry.getKey();
      final KalininMiljukovType km = entry.getValue();

      final String label = FeatureHelper.getAnnotationValue( channel, IAnnotation.ANNO_LABEL );
      monitor.subTask( String.format( "%s...", label ) );

      try
      {
        final IStatus result = updateChannelData( label, channel, km );
        problems.add( result );
      }
      catch( final Exception e )
      {
        final Status error = new Status( IStatus.OK, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.22", label ) );
        KMPlugin.getDefault().getLog().log( error );
        problems.add( error );
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    final IStatus[] calculationStati = problems.toArray( new IStatus[problems.size()] );
    final IStatus calculationStatus = new MultiStatus( KMPlugin.getID(), 0, calculationStati, "KM parameters are determined", null );
    return calculationStatus;
  }

  private IStatus updateChannelData( final String label, final KMChannel kmChannel, final KalininMiljukovType km ) throws Exception
  {
    final IStatus[] result = calculateChannel( kmChannel, km );
    return new MultiStatus( KMPlugin.getID(), 0, result, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.20", label ), null );
  }

  private IStatus[] calculateChannel( final KMChannel kmChannel, final KalininMiljukovType km ) throws Exception
  {
    final IFeatureType kmFT = m_workspace.getFeatureType( KMChannel.FEATURE_KM_CHANNEL );
    final IFeatureType kmPaFT = m_workspace.getFeatureType( KMParameter.FEATURE_KM_PARAMETER );

    final IPropertyType kmKMStartPT = kmFT.getProperty( KMChannel.PROP_KMSTART );
    final IPropertyType kmKMEndPT = kmFT.getProperty( KMChannel.PROP_KMEND );

    final IPropertyType qrkPT = kmPaFT.getProperty( NaModelConstants.KM_CHANNEL_QRK_PROP );
    final IPropertyType rkvT = kmPaFT.getProperty( NaModelConstants.KM_CHANNEL_RKV_PROP );
    final IPropertyType rnvPT = kmPaFT.getProperty( NaModelConstants.KM_CHANNEL_RNV_PROP );
    final IPropertyType cPT = kmPaFT.getProperty( NaModelConstants.KM_CHANNEL_C_PROP );

    final Double kmStart = km.getKmStart();
    final Double kmEnd = km.getKmEnd();
    // FIXME: should already have been validated in dialog
    if( kmStart == null || kmEnd == null )
    {
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.35" ) ); //$NON-NLS-1$
      return new IStatus[] { status };
    }
    if( kmStart.compareTo( kmEnd ) > 0 )
    {
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.36" ) ); //$NON-NLS-1$
      return new IStatus[] { status };
    }
    // TODO: also better validate in dialog
    final File[] files = getFiles( km );
    if( files.length == 0 )
    {
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.37" ) ); //$NON-NLS-1$
      return new IStatus[] { status };
    }

    final ProfileDataSet profileSet = ProfileFactory.createProfileSet( files, kmStart, kmEnd );
    final IKMValue[] values = profileSet.getKMValues();

    m_featureChanges.add( new FeatureChange( kmChannel, kmKMStartPT, km.getKmStart() ) );
    m_featureChanges.add( new FeatureChange( kmChannel, kmKMEndPT, km.getKmEnd() ) );

    final IFeatureBindingCollection<KMParameter> kmParameter = kmChannel.getParameters();
    // TODO: make it more general - not reduced to 5
    // TODO: always remove all existing features and add new ones
    final int max = 5;
    if( kmParameter.size() != max )
      return new IStatus[] { new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.38" ) ) };

    final IStatus[] paramLog = new IStatus[5];
    for( int i = 0; i < kmParameter.size(); i++ )
    {
      final KMParameter kmParameterFE = kmParameter.get( i );
      final IKMValue value = values[i];
      paramLog[i] = new Status( IStatus.OK, KMPlugin.getID(), String.format( "%d. %s", i + 1, value ) );//$NON-NLS-1$

      final BigDecimal k = new BigDecimal( value.getK() ).setScale( 4, BigDecimal.ROUND_HALF_UP );
      final BigDecimal n = new BigDecimal( value.getN() ).setScale( 4, BigDecimal.ROUND_HALF_UP );
      final BigDecimal kForeland = new BigDecimal( value.getKForeland() ).setScale( 2, BigDecimal.ROUND_HALF_UP );
      final BigDecimal nForeland = new BigDecimal( value.getNForeland() ).setScale( 2, BigDecimal.ROUND_HALF_UP );
      final BigDecimal qSum = new BigDecimal( value.getQSum() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
      final BigDecimal alpha = new BigDecimal( value.getAlpha() ).setScale( 3, BigDecimal.ROUND_HALF_UP );

      m_featureChanges.add( new FeatureChange( kmParameterFE, KMParameter.PROP_RKF, k.doubleValue() ) ); //$NON-NLS-1$
      m_featureChanges.add( new FeatureChange( kmParameterFE, rkvT, kForeland.doubleValue() ) ); //$NON-NLS-1$
      m_featureChanges.add( new FeatureChange( kmParameterFE, KMParameter.PROP_RNF, n.doubleValue() ) ); //$NON-NLS-1$
      m_featureChanges.add( new FeatureChange( kmParameterFE, rnvPT, nForeland.doubleValue() ) ); //$NON-NLS-1$
      m_featureChanges.add( new FeatureChange( kmParameterFE, qrkPT, qSum.doubleValue() ) ); //$NON-NLS-1$
      m_featureChanges.add( new FeatureChange( kmParameterFE, cPT, alpha.doubleValue() ) ); //$NON-NLS-1$
    }

    return paramLog;
  }

  private File[] getFiles( final KalininMiljukovType km )
  {
    final List<File> list = new ArrayList<File>();
    final List<Profile> profiles = km.getProfile();
    for( final Profile profile : profiles )
    {
      if( profile.isEnabled() )
      {
        final String path = profile.getFile();
        final File file = new File( path );
        if( file.canRead() )
          list.add( file );
      }
    }

    return list.toArray( new File[list.size()] );
  }

}
