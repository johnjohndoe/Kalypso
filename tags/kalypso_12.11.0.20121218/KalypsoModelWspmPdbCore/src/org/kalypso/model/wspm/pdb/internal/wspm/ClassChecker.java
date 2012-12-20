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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.TreeSet;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.classifications.IClassificationClass;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.pdb.db.mapping.IPdbClass;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Checks if local and remote classes are different.
 *
 * @author Gernot Belger
 */
public class ClassChecker
{
  private static final String STR_EMPTY = Messages.getString("ClassChecker_0"); //$NON-NLS-1$

  private final IStatusCollector m_stati = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final Collection<Roughness> m_roughnessToCheck = new TreeSet<>();

  private final Collection<Vegetation> m_vegetationToCheck = new TreeSet<>();

  private final IProfileFeature[] m_profiles;

  public ClassChecker( final IProfileFeature[] profiles )
  {
    m_profiles = profiles;
  }

  public void addRoughness( final Roughness roughness )
  {
    m_roughnessToCheck.add( roughness );
  }

  public void addVegetation( final Vegetation vegetation )
  {
    m_vegetationToCheck.add( vegetation );
  }

  public IStatus execute( )
  {
    final IWspmClassification classification = findClassification();
    if( classification == null )
      m_stati.add( IStatus.WARNING, Messages.getString("ClassChecker_1") ); //$NON-NLS-1$
    else
    {
      checkRoughness( classification );
      checkVegetation( classification );
    }

    return m_stati.asMultiStatusOrOK( Messages.getString("ClassChecker_2") ); //$NON-NLS-1$
  }

  private IWspmClassification findClassification( )
  {
    if( ArrayUtils.isEmpty( m_profiles ) )
      return null;

    final GMLWorkspace workspace = m_profiles[0].getWorkspace();
    if( workspace == null )
      return null;

    final Feature rootFeature = workspace.getRootFeature();
    if( !(rootFeature instanceof TuhhWspmProject) )
      return null;

    final TuhhWspmProject project = (TuhhWspmProject) rootFeature;
    return project.getClassificationMember();
  }

  private void checkRoughness( final IWspmClassification classification )
  {
    for( final Roughness roughness : m_roughnessToCheck )
    {
      final String classID = roughness.getId().getName();
      final IRoughnessClass roughnessClass = classification.findRoughnessClass( classID );

      if( roughnessClass == null )
      {
        // should never happen
      }
      else
      {
        checkValues( roughnessClass.getKsValue(), roughness.getKValue(), roughnessClass, roughness, "ks" ); //$NON-NLS-1$
        checkValues( roughnessClass.getKstValue(), roughness.getKstValue(), roughnessClass, roughness, "kst" ); //$NON-NLS-1$
      }
    }
  }

  private void checkVegetation( final IWspmClassification classification )
  {
    for( final Vegetation vegetation : m_vegetationToCheck )
    {
      final String classID = vegetation.getId().getName();
      final IVegetationClass vegetationClass = classification.findVegetationClass( classID );

      if( vegetationClass == null )
      {
        // should never happen
      }
      else
      {
        checkValues( vegetationClass.getAx(), vegetation.getAx(), vegetationClass, vegetation, "ax" ); //$NON-NLS-1$
        checkValues( vegetationClass.getAy(), vegetation.getAy(), vegetationClass, vegetation, "ay" ); //$NON-NLS-1$
        checkValues( vegetationClass.getDp(), vegetation.getDp(), vegetationClass, vegetation, "dp" ); //$NON-NLS-1$
      }
    }
  }

  /**
   * Check if two class values are different: if yes, add warning
   */
  private void checkValues( final BigDecimal local, final BigDecimal remote, final IClassificationClass localClass, final IPdbClass remoteClass, final String valueLabel )
  {
    if( local == null && remote == null )
      return;

    if( local == null && remote != null )
    {
      addWarning( local, remote, remoteClass.getLabel(), valueLabel );
      return;
    }

    if( local != null && remote == null )
    {
      addWarning( local, remote, localClass.getDescription(), valueLabel );
      return;
    }

    /* Bring to same scale to avoid false warnings */
    final int localScale = local.scale();
    final int remoteScale = remote.scale();
    final int maxScale = Math.max( localScale, remoteScale );
    final BigDecimal localScaled = local.setScale( maxScale );
    final BigDecimal remoteScaled = remote.setScale( maxScale );

    if( localScaled.compareTo( remoteScaled ) == 0 )
      return;

    addWarning( local, remote, remoteClass.getLabel(), valueLabel );
  }

  private void addWarning( final BigDecimal local, final BigDecimal remote, final String classLabel, final String valueLabel )
  {
    final String valuesText = formatValues( local, remote );
    m_stati.add( IStatus.WARNING, Messages.getString("ClassChecker_3"), null, classLabel, valueLabel, valuesText ); //$NON-NLS-1$
  }

  private String formatValues( final BigDecimal local, final BigDecimal remote )
  {
    final String localText = local == null ? STR_EMPTY : local.toString();
    final String remoteText = remote == null ? STR_EMPTY : remote.toString();

    return String.format( Messages.getString("ClassChecker_4"), localText, remoteText ); //$NON-NLS-1$
  }
}