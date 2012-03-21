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
package org.kalypso.model.km.internal.core;

import java.math.BigDecimal;
import java.util.HashMap;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.KMParameter;
import org.kalypso.model.km.internal.KMPlugin;
import org.kalypso.model.km.internal.binding.KMChannelElement;
import org.kalypso.model.km.internal.i18n.Messages;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import com.google.common.base.Objects;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;

/**
 * @author Gernot Belger
 */
public class KMUpdateOperation implements ICoreRunnableWithProgress
{
  private final CompositeCommand m_commands = new CompositeCommand( Messages.getString( "KMUpdateOperation.0" ) ); //$NON-NLS-1$

  private final KMChannelElement[] m_channels;

  private final CommandableWorkspace m_workspace;

  public KMUpdateOperation( final CommandableWorkspace workspace, final KMChannelElement[] channels )
  {
    m_workspace = workspace;
    m_channels = channels;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.21" ), m_channels.length + 10 ); //$NON-NLS-1$

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

  private IStatus calculateKM( final IProgressMonitor monitor )
  {
    final StatusCollector problems = new StatusCollector( KMPlugin.getID() );

    for( final KMChannelElement element : m_channels )
    {
      final KMChannel channel = element.getKMChannel();

      final String label = FeatureHelper.getAnnotationValue( channel, IAnnotation.ANNO_LABEL );
      monitor.subTask( String.format( "%s...", label ) ); //$NON-NLS-1$

      try
      {
        final IStatus result = updateChannelData( label, element );
        problems.add( result );
      }
      catch( final CoreException ce )
      {
        final IStatus status = ce.getStatus();
        final String message = Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.22", label ); //$NON-NLS-1$
        problems.add( new MultiStatus( KMPlugin.getID(), 0, new IStatus[] { status }, message, null ) );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final String message = Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.22", label ); //$NON-NLS-1$
        problems.add( IStatus.ERROR, message, e );
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    return problems.asMultiStatus( Messages.getString( "KMUpdateOperation_3" ) ); //$NON-NLS-1$
  }

  private IStatus updateChannelData( final String label, final KMChannelElement element ) throws Exception
  {
    final IStatus[] result = calculateChannel( element );
    return new MultiStatus( KMPlugin.getID(), 0, result, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.20", label ), null ); //$NON-NLS-1$
  }

  private IStatus[] calculateChannel( final KMChannelElement element ) throws Exception
  {
    // REMARK: At the moment, it is hard-coded in Kalypso-NA to use exactly 5 parameters
    // We should change that in Kalypso-NA and get number of parameters from the user
    final int paramCount = 5;

    final IFeatureType kmFT = GMLSchemaUtilities.getFeatureTypeQuiet( KMChannel.FEATURE_KM_CHANNEL );

    final IPropertyType kmKMStartPT = kmFT.getProperty( KMChannel.PROP_KMSTART );
    final IPropertyType kmKMEndPT = kmFT.getProperty( KMChannel.PROP_KMEND );

    final KalininMiljukovType km = element.getKMType();

    final BigDecimal kmStart = km.getKmStart();
    final BigDecimal kmEnd = km.getKmEnd();
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

    final ProfileDataSet profileSet = element.getEnabledSet();
    if( profileSet.getAllProfiles().length == 0 )
    {
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.37" ) ); //$NON-NLS-1$
      return new IStatus[] { status };
    }

    final KMChannel kmChannel = element.getKMChannel();
    final IFeatureBindingCollection<KMParameter> kmParameter = kmChannel.getParameters();

    final double startKM = ((Number) Objects.firstNonNull( kmChannel.getKMStart(), kmStart )).doubleValue();
    final double endKM = ((Number) Objects.firstNonNull( kmChannel.getKMEnd(), kmEnd )).doubleValue();

    final double lengthOfStrand = Math.abs( startKM - endKM ) * 1000.0;

    final IKMValue[] values = profileSet.getKMValues( lengthOfStrand, paramCount );
    final IStatusCollector paramLog = new StatusCollector( KMPlugin.getID() );
    final double qBordvoll = profileSet.getQBordvoll();
    paramLog.add( IStatus.OK, Messages.getString( "KMUpdateOperation.1" ), null, qBordvoll ); //$NON-NLS-1$

    addCommand( new ChangeFeatureCommand( kmChannel, kmKMStartPT, kmStart.doubleValue() ) );
    addCommand( new ChangeFeatureCommand( kmChannel, kmKMEndPT, kmEnd.doubleValue() ) );
    removeExistingParameters( kmParameter );

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

  private IStatus writeValue( final KMChannel channel, final IKMValue value, final int index )
  {
    final IStatusCollector valueLog = new StatusCollector( KMPlugin.getID() );

    final IFeatureType featureType = GMLSchemaUtilities.getFeatureTypeQuiet( KMParameter.FEATURE_KM_PARAMETER );
    final IRelationType parameterType = (IRelationType) channel.getFeatureType().getProperty( KMChannel.MEMBER_PARAMETER );

    final AddFeatureCommand addCommand = new AddFeatureCommand( m_workspace, featureType, channel, parameterType, -1, new HashMap<IPropertyType, Object>(), null, -1 );
    m_commands.addCommand( addCommand );

    final double k = roundValue( value.getK(), 4 );
    validate( k, Messages.getString( "KMUpdateOperation_6" ), valueLog ); //$NON-NLS-1$

    final double n = roundValue( value.getN(), 4 );
    final double nValid = validateN( n, Messages.getString( "KMUpdateOperation_7" ), valueLog ); //$NON-NLS-1$

    final double kForeland = roundValue( value.getKForeland(), 4 );
    validate( kForeland, Messages.getString( "KMUpdateOperation_8" ), valueLog ); //$NON-NLS-1$

    final double nForeland = roundValue( value.getNForeland(), 4 );
    final double nForelandValid = validateN( nForeland, Messages.getString( "KMUpdateOperation_9" ), valueLog ); //$NON-NLS-1$

    // REMARK: using the lower sum value: this is in sync with Kalypso-NA implementation: Kalypso-NA uses the
    // coefficients of the next lower q in the parameter set.
    final double qSum = roundValue( value.getLowerQ(), 3 );
    validate( qSum, Messages.getString( "KMUpdateOperation_10" ), valueLog ); //$NON-NLS-1$

    final double alpha = roundValue( value.getAlpha(), 3 );
    validate( alpha, Messages.getString( "KMUpdateOperation_11" ), valueLog ); //$NON-NLS-1$

    addCommand.addProperty( KMParameter.PROP_QRK, qSum );
    addCommand.addProperty( KMParameter.PROP_RKF, k );
    addCommand.addProperty( KMParameter.PROP_RNF, nValid );
    addCommand.addProperty( KMParameter.PROP_RKV, kForeland );
    addCommand.addProperty( KMParameter.PROP_RNV, nForelandValid );
    addCommand.addProperty( KMParameter.PROP_C, alpha );

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
}