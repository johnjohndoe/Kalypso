/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.filterdialog.widgets;

import java.util.TreeSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsCOMPOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;

/**
 * @author kuepfer
 */
public class FilterCompositeFactory extends ModellEventProviderAdapter
{
  private static FilterCompositeFactory m_factory = new FilterCompositeFactory();

  public static FilterCompositeFactory getInstance( )
  {
    return m_factory;
  }

  public static AbstractFilterComposite createFilterElementComposite( final Composite parent, final IErrorMessageReciever errorMessageReciever, final Operation operation, final TreeSet<String> supportedOperations, final IFeatureType ft, Feature spatialOperator )
  {
    AbstractFilterComposite c = null;

    if( operation != null )
    {
      String operatorName = operation.getOperatorName();
      if( operation instanceof PropertyIsCOMPOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter Comperator";
        ((Group) parent).setText( "Eigenschaften-" + operatorName );
        c = new PropertyIsCOMPOperationComposite( parent, SWT.NULL, (PropertyIsCOMPOperation) operation, supportedOperations, errorMessageReciever, ft );
      }
      else if( operation instanceof PropertyIsLikeOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter IsLike Operator";
        ((Group) parent).setText( "Eigenschaften-" + operatorName );
        c = new PropertyIsLikeOperationComposite( parent, SWT.NULL, (PropertyIsLikeOperation) operation, errorMessageReciever, ft );

      }
      else if( operation instanceof PropertyIsBetweenOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter IsBetween Operator";
        ((Group) parent).setText( "Eigenschaften-" + operatorName );
        c = new PropertyIsBetweenComposite( parent, SWT.NULL, (PropertyIsBetweenOperation) operation, errorMessageReciever, ft );

      }
      else if( operation instanceof SpatialOperation )
      {
        if( operatorName == null )
          operatorName = "Unbekannter Spatial Operator";
        ((Group) parent).setText( "Eigenschaften-" + operatorName );
        c = new SpatialComposite( parent, SWT.NULL, (SpatialOperation) operation, errorMessageReciever, ft, spatialOperator );
      }
    }
    return c;
  }
}
