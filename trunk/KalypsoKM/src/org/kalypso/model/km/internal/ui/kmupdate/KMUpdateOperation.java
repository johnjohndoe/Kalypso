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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.KMParameter;
import org.kalypso.model.km.internal.KMPlugin;
import org.kalypso.model.km.internal.core.IKMValue;
import org.kalypso.model.km.internal.core.ProfileDataSet;
import org.kalypso.model.km.internal.core.ProfileObservationReader;
import org.kalypso.model.km.internal.i18n.Messages;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author Gernot Belger
 */
public class KMUpdateOperation implements ICoreRunnableWithProgress
{
  private final CompositeCommand m_commands = new CompositeCommand( "KM-Parameter erzeugen" );

  private final Map<KMChannel, KalininMiljukovType> m_channels;

  private final CommandableWorkspace m_workspace;

  public KMUpdateOperation( final CommandableWorkspace workspace, final Map<KMChannel, KalininMiljukovType> channels )
  {
    m_workspace = workspace;
    m_channels = channels;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.21" ), m_channels.size() + 10 ); //$NON-NLS-1$

    final IStatus calculationStatus = calculateKM( monitor );
    if( calculationStatus.matches( IStatus.ERROR ) )
      return calculationStatus;

    try
    {
      monitor.subTask( Messages.getString( "KMUpdateOperation_0" ) ); //$NON-NLS-1$
      m_workspace.postCommand( m_commands );
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
    final StatusCollector problems = new StatusCollector( KMPlugin.getID() );

    for( final Entry<KMChannel, KalininMiljukovType> entry : m_channels.entrySet() )
    {
      final KMChannel channel = entry.getKey();
      final KalininMiljukovType km = entry.getValue();

      final String label = FeatureHelper.getAnnotationValue( channel, IAnnotation.ANNO_LABEL );
      monitor.subTask( String.format( "%s...", label ) ); //$NON-NLS-1$

      try
      {
        final IStatus result = updateChannelData( label, channel, km );
        problems.add( result );
      }
      catch( final Exception e )
      {
        problems.add( IStatus.ERROR, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.22", label ), e ); //$NON-NLS-1$
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    return problems.asMultiStatus( Messages.getString( "KMUpdateOperation_3" ) ); //$NON-NLS-1$
  }

  private IStatus updateChannelData( final String label, final KMChannel kmChannel, final KalininMiljukovType km ) throws Exception
  {
    final IStatus[] result = calculateChannel( kmChannel, km );
    return new MultiStatus( KMPlugin.getID(), 0, result, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.20", label ), null ); //$NON-NLS-1$
  }

  private IStatus[] calculateChannel( final KMChannel kmChannel, final KalininMiljukovType km ) throws Exception
  {
    final IFeatureType kmFT = m_workspace.getFeatureType( KMChannel.FEATURE_KM_CHANNEL );

    final IPropertyType kmKMStartPT = kmFT.getProperty( KMChannel.PROP_KMSTART );
    final IPropertyType kmKMEndPT = kmFT.getProperty( KMChannel.PROP_KMEND );

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
    final IPath[] paths = getPaths( km );
    if( paths.length == 0 )
    {
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.37" ) ); //$NON-NLS-1$
      return new IStatus[] { status };
    }

    // HACK: In the profile observation set all files are the same...
    final ProfileObservationReader reader = new ProfileObservationReader( paths[0] );
    final ProfileDataSet profileSet = reader.getDataSet( 1000d * kmStart, 1000d * kmEnd );

    addCommand( new ChangeFeatureCommand( kmChannel, kmKMStartPT, km.getKmStart() ) );
    addCommand( new ChangeFeatureCommand( kmChannel, kmKMEndPT, km.getKmEnd() ) );

    final IFeatureBindingCollection<KMParameter> kmParameter = kmChannel.getParameters();

    removeExistingParameters( kmParameter );

    // TODO: get number of parameters from user
    final int paramCount = 5;

    final IKMValue[] values = profileSet.getKMValues( paramCount );

    final IStatusCollector paramLog = new StatusCollector( KMPlugin.getID() );
    for( int i = 0; i < paramCount; i++ )
    {
      final IKMValue value = values[i];
      final IStatus log = writeValue( kmChannel, value, i );
      paramLog.add( log );
    }

    return paramLog.getAllStati();
  }

  private void removeExistingParameters( final IFeatureBindingCollection<KMParameter> params )
  {
    for( final KMParameter kmParameter : params )
      addCommand( new DeleteFeatureCommand( kmParameter ) );
  }

  private void addCommand( final ICommand command )
  {
    m_commands.addCommand( command );
  }

  private IStatus writeValue( final KMChannel kmChannel, final IKMValue value, final int index )
  {
    final IStatusCollector valueLog = new StatusCollector( KMPlugin.getID() );

    final IFeatureType kmParamFT = m_workspace.getGMLSchema().getFeatureType( KMParameter.FEATURE_KM_PARAMETER );
    final IRelationType kmParamRT = (IRelationType) kmChannel.getFeatureType().getProperty( KMChannel.MEMBER_PARAMETER );

    final AddFeatureCommand addCommand = new AddFeatureCommand( m_workspace, kmParamFT, kmChannel, kmParamRT, -1, -1 );
    m_commands.addCommand( addCommand );

    final double k = roundValue( value.getK(), 4 );
    validate( k, Messages.getString( "KMUpdateOperation_6" ), valueLog ); //$NON-NLS-1$

    final double n = roundValue( value.getN(), 4 );
    final double nValid = validateN( n, Messages.getString( "KMUpdateOperation_7" ), valueLog ); //$NON-NLS-1$

    final double kForeland = roundValue( value.getKForeland(), 4 );
    validate( kForeland, Messages.getString( "KMUpdateOperation_8" ), valueLog ); //$NON-NLS-1$

    final double nForeland = roundValue( value.getNForeland(), 4 );
    final double nForelandValid = validateN( nForeland, Messages.getString( "KMUpdateOperation_9" ), valueLog ); //$NON-NLS-1$

    final double qSum = roundValue( value.getQSum(), 3 );
    validate( qSum, Messages.getString( "KMUpdateOperation_10" ), valueLog ); //$NON-NLS-1$

    final double alpha = roundValue( value.getAlpha(), 3 );
    validate( alpha, Messages.getString( "KMUpdateOperation_11" ), valueLog ); //$NON-NLS-1$

    addCommand.setProperty( KMParameter.PROP_RKF, k );
    addCommand.setProperty( KMParameter.PROP_RKV, kForeland );
    addCommand.setProperty( KMParameter.PROP_RNF, nValid );
    addCommand.setProperty( KMParameter.PROP_RNV, nForelandValid );
    addCommand.setProperty( KMParameter.PROP_QRK, qSum );
    addCommand.setProperty( KMParameter.PROP_C, alpha );

    final String msg = String.format( "%d. %s", index + 1, value );//$NON-NLS-1$
    return valueLog.asMultiStatus( msg );
  }

  private double validateN( final double n, final String label, final IStatusCollector valueLog )
  {
    validate( n, label, valueLog );

    if( !Double.isNaN( n ) && n > 30 )
    {
      valueLog.add( IStatus.INFO, Messages.getString( "KMUpdateOperation_12" ), null, label ); //$NON-NLS-1$
      return 30;
    }

    return n;
  }

  private void validate( final double k, final String label, final IStatusCollector paramLog )
  {
    if( Double.isNaN( k ) )
      paramLog.add( IStatus.WARNING, Messages.getString( "KMUpdateOperation_13" ), null, label ); //$NON-NLS-1$
  }

  private double roundValue( final double value, final int scale )
  {
    if( Double.isNaN( value ) || Double.isInfinite( value ) )
      return Double.NaN;

    final BigDecimal decimal = new BigDecimal( value ).setScale( scale, BigDecimal.ROUND_HALF_UP );
    return decimal.doubleValue();
  }

  private IPath[] getPaths( final KalininMiljukovType km )
  {
    final List<IPath> list = new ArrayList<IPath>();
    final List<Profile> profiles = km.getProfile();
    for( final Profile profile : profiles )
    {
      if( profile.isEnabled() )
      {
        final Path path = new Path( profile.getFile() );
        list.add( path );
      }
    }

    return list.toArray( new IPath[list.size()] );
  }
}