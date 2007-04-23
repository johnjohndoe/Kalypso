/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.featureview.modfier;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ComboBoxCellEditor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.control.ComboFeatureControl;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider2;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author belger
 */
public class ComboBoxModifier implements IFeatureModifier
{
  private static final String NO_LINK_STRING = "<kein Link>";

  private static final Object NULL_LINK = new Object();

  private final IRelationType m_rt;

  private Feature m_feature;

  private Map<String, Object> m_entries = new HashMap<String, Object>();

  private ComboBoxCellEditor m_comboBoxCellEditor = new ComboBoxCellEditor();

  public ComboBoxModifier( final IRelationType ftp )
  {
    m_rt = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getValue(org.kalypsodeegree.model.feature.Feature)
   */
  public Object getValue( final Feature f )
  {
    m_feature = f;
    m_entries.clear();
    if( !m_rt.isInlineAble() && m_rt.isLinkAble() )
    {
      /* Null entry to delete link if this is allowed */
      if( m_rt.isNillable() )
        m_entries.put( NO_LINK_STRING, NULL_LINK ); //$NON-NLS-1$

      /* Find all substituting features. */
      final Feature feature = getFeature();

      final GMLWorkspace workspace = feature.getWorkspace();

      final Feature[] features = ComboFeatureControl.collectReferencableFeatures( workspace, m_feature, m_rt );

      final GMLEditorLabelProvider2 labelProvider = new GMLEditorLabelProvider2();

      for( final Feature foundFeature : features )
      {
        if( foundFeature instanceof XLinkedFeature_Impl )
          m_entries.put( labelProvider.getText( foundFeature ), foundFeature );
        else
          m_entries.put( labelProvider.getText( foundFeature ), foundFeature.getId() );
      }
    }
    final Collection<String> entries = m_entries.keySet();
    m_comboBoxCellEditor.setItems( entries.toArray( new String[entries.size()] ) );

    final List<Object> values = new ArrayList<Object>( m_entries.values() );
    final Object property = f.getProperty( m_rt );
    return values.indexOf( property ) + (m_rt.isNillable() ? 1 : 0);
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#parseInput(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.Object)
   */
  public Object parseInput( final Feature f, final Object value )
  {
    final Collection<Object> values = m_entries.values();
    int counter = ((Integer) value).intValue();
    if( m_rt.isNillable() && counter-- == 0 )
    {
      return null;
    }
    for( final Object object : values )
    {
      if( counter-- == 0 )
        return object;
    }
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#createCellEditor(org.eclipse.swt.widgets.Composite)
   */
  public CellEditor createCellEditor( final Composite parent )
  {
    m_comboBoxCellEditor.create( parent );
    return m_comboBoxCellEditor;
  }

  protected Feature getFeature( )
  {
    return m_feature;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellEditorValidator#isValid(java.lang.Object)
   */
  public String isValid( final Object value )
  {
    return null; // null means vaild
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getFeatureTypeProperty()
   */
  public IPropertyType getFeatureTypeProperty( )
  {
    return m_rt;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getLabel(org.kalypsodeegree.model.feature.Feature)
   */
  public String getLabel( final Feature f )
  {
    // TODO: GUITypeHandler konsequent einsetzen
    // besser: abhängig von der IPropertyType etwas machen
    final IPropertyType ftp = getFeatureTypeProperty();
    Object fprop = f.getProperty( ftp );
    if( fprop != null )
    {
      return fprop.toString();
    }
    else
      return NO_LINK_STRING;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getImage(org.kalypsodeegree.model.feature.Feature)
   */
  public Image getImage( final Feature f )
  {
    // Todo: button image
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#dispose()
   */
  public void dispose( )
  {
    // nichts zu tun
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#equals(java.lang.Object, java.lang.Object)
   */
  public boolean equals( final Object newData, final Object oldData )
  {
    return newData.equals( oldData );
  }
}