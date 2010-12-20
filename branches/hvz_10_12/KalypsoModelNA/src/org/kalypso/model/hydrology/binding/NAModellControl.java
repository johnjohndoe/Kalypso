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
package org.kalypso.model.hydrology.binding;

import java.util.Date;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for {org.kalypso.namodell.control}NAModellControl.
 * 
 * @author Gernot Belger
 */
public class NAModellControl extends Feature_Impl
{
  public static final String NS_NACONTROL = NaModelConstants.NS_NACONTROL;

  private static final QName PROP_INITIALVALUEDATE = new QName( NS_NACONTROL, "InitialValueDate" ); //$NON-NLS-1$

  private static final QName PROP_TMP = new QName( NS_NACONTROL, "tmp" ); //$NON-NLS-1$

  private static final QName PRE_PROP = new QName( NS_NACONTROL, "pre" ); //$NON-NLS-1$

  private static final QName PROP_SCH = new QName( NS_NACONTROL, "sch" ); //$NON-NLS-1$

  private static final QName PROP_BOF = new QName( NS_NACONTROL, "bof" ); //$NON-NLS-1$

  private static final QName PROP_BSP = new QName( NS_NACONTROL, "bsp" ); //$NON-NLS-1$

  private static final QName PROP_GWS = new QName( NS_NACONTROL, "gws" ); //$NON-NLS-1$

  private static final QName PROP_QGS = new QName( NS_NACONTROL, "qgs" ); //$NON-NLS-1$

  private static final QName PROP_QGG = new QName( NS_NACONTROL, "qgg" ); //$NON-NLS-1$

  private static final QName PROP_QNA = new QName( NS_NACONTROL, "qna" ); //$NON-NLS-1$

  private static final QName PROP_QIF = new QName( NS_NACONTROL, "qif" ); //$NON-NLS-1$

  private static final QName PROP_QVS = new QName( NS_NACONTROL, "qvs" ); //$NON-NLS-1$

  private static final QName PROP_QBS = new QName( NS_NACONTROL, "qbs" ); //$NON-NLS-1$

  private static final QName PROP_QT1 = new QName( NS_NACONTROL, "qt1" ); //$NON-NLS-1$

  private static final QName PROP_QTG = new QName( NS_NACONTROL, "qtg" ); //$NON-NLS-1$

  private static final QName PROP_QGW = new QName( NS_NACONTROL, "qgw" ); //$NON-NLS-1$

  private static final QName PROP_VET = new QName( NS_NACONTROL, "vet" ); //$NON-NLS-1$

  // Stil in model, but should not be used any more, as Mulden-Rigolen are now handled via SUDS.
  //  private static final QName NACONTROL_QMR_PROP = new QName( NS_NACONTROL, "qmr" ); //$NON-NLS-1$

  private static final QName PROP_HYD = new QName( NS_NACONTROL, "hyd" ); //$NON-NLS-1$

  private static final QName PROP_BIL = new QName( NS_NACONTROL, "bil" ); //$NON-NLS-1$

  private static final QName PROP_NMQ = new QName( NS_NACONTROL, "nmq" ); //$NON-NLS-1$

  private static final QName PROP_SPI = new QName( NS_NACONTROL, "spi" ); //$NON-NLS-1$

  private static final QName PROP_SUP = new QName( NS_NACONTROL, "sup" ); //$NON-NLS-1$


  public NAModellControl( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public boolean doGenerateTMP( )
  {
    return getBoolean( PROP_TMP, false );
  }

  public boolean doGeneratePRE( )
  {
    return getBoolean( PRE_PROP, false );
  }

  public boolean doGenerateSCH( )
  {
    return getBoolean( PROP_SCH, false );
  }

  public boolean doGenerateBOF( )
  {
    return getBoolean( PROP_BOF, false );
  }

  public boolean doGenerateBSP( )
  {
    return getBoolean( PROP_BSP, false );
  }

  public boolean doGenerateGWS( )
  {
    return getBoolean( PROP_GWS, false );
  }

  public boolean doGenerateQGS( )
  {
    return getBoolean( PROP_QGS, false );
  }

  public boolean doGenerateQGG( )
  {
    return getBoolean( PROP_QGG, false );
  }

  public boolean doGenerateQNA( )
  {
    return getBoolean( PROP_QNA, false );
  }

  public boolean doGenerateQIF( )
  {
    return getBoolean( PROP_QIF, false );
  }

  public boolean doGenerateQVS( )
  {
    return getBoolean( PROP_QVS, false );
  }

  public boolean doGenerateQBS( )
  {
    return getBoolean( PROP_QBS, false );
  }

  public boolean doGenerateQT1( )
  {
    return getBoolean( PROP_QT1, false );
  }

  public boolean doGenerateQTG( )
  {
    return getBoolean( PROP_QTG, false );
  }

  public boolean doGenerateQGW( )
  {
    return getBoolean( PROP_QGW, false );
  }

  public boolean doGenerateVET( )
  {
    return getBoolean( PROP_VET, false );
  }

  public boolean doGenerateHYD( )
  {
    return getBoolean( PROP_HYD, false );
  }
  public boolean doGenerateBIL( )
  {
    return getBoolean( PROP_BIL, false );
  }

  public boolean doGenerateNMQ( )
  {
    return getBoolean( PROP_NMQ, false );
  }

  public boolean doGenerateSPI( )
  {
    return getBoolean( PROP_SPI, false );
  }

  public boolean doGenerateSUP( )
  {
    return getBoolean( PROP_SUP, false );
  }

  /**
   * Get initial dates that should be written.<br/>The dates will be returned sorted.
   */
  public Date[] getInitialDatesToBeWritten(  )
  {
    final List< ? > dateList = (List< ? >) getProperty( PROP_INITIALVALUEDATE );
    if( dateList == null )
      return new Date[0];

    final SortedSet<Date> dateWriteSet = new TreeSet<Date>();
    for( final Object object : dateList )
    {
      final InitialValues fe = (InitialValues) object;
      if( fe.doWrite() )
      {
        final Date initialDate = fe.getInitialDate();
        dateWriteSet.add( initialDate );
      }
    }

    return dateWriteSet.toArray( new Date[dateWriteSet.size()] );
  }
}
