package org.kalypso.ogc.gml.featureview;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;


/**
 * @author belger
 */
public interface IFeatureModifier extends ICellEditorValidator
{
  public void dispose();
  
  public String getLabel( final Feature f );
  
  public Image getImage( final Feature f );
  
  /** macht aus dem Feature ein (anzeigbares) Objekt */
  public Object getValue( final Feature f );
  
  /** macht aus dem (editierten) Objekt ein Object, welches dem Feature wider
   * als Property gesetzt werden kann */
  public Object parseInput( final Feature f, final Object value );
  
  public CellEditor createCellEditor( final Composite parent );
  
  public String isValid( final Object value );

  public FeatureTypeProperty getFeatureTypeProperty();
}
