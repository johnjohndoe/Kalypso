package org.kalypso.ogc.gml.featureview.modfier;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;

/**
 * @author belger
 */
public class StringModifier implements IFeatureModifier
{
  private final DateFormat DATE_FORMATTER = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );
  
  private final FeatureTypeProperty m_ftp;
  
  public StringModifier( final FeatureTypeProperty ftp )
  {
    m_ftp = ftp;
  }
  
  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#dispose()
   */
  public void dispose()
  {
    // nix zu tun
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getValue(org.deegree.model.feature.Feature)
   */
  public Object getValue( final Feature f )
  {
    final String type = m_ftp.getType();
    final Object data = f.getProperty( m_ftp.getName() );

    if( data == null )
      return "";

    if( "java.util.Date".equals( type ) )
      return DATE_FORMATTER.format( data );

    return data.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#parseInput(org.deegree.model.feature.Feature, java.lang.Object)
   */
  public Object parseInput( final Feature f, final Object value )
  {
    final String text = value.toString();
    if( text.length() == 0 )
      return null;
    
    try
    {
      return parseData( text );
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
    }
    catch( final ParseException e )
    {
      e.printStackTrace();
    }

    return null;
  }

  private Object parseData( final String text ) throws ParseException
  {
    final String typeName = m_ftp.getType();
    if( typeName.equals( "java.lang.String" ) )
      return text;
    if( typeName.equals( "java.lang.Double" ) )
      return new Double( text );
    if( typeName.equals( "java.lang.Integer" ) )
      return new Integer( text );
    if( typeName.equals( "java.lang.Float" ) )
      return new Float( text );
    if( typeName.equals( "java.lang.Long" ) )
      return new Long( text );
    if( typeName.equals( "java.lang.Boolean" ) )
      return new Boolean( text );
    if( typeName.equals( "java.util.Date" ) )
      return DATE_FORMATTER.parse( text );
    
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#createCellEditor(org.eclipse.swt.widgets.Composite)
   */
  public CellEditor createCellEditor( final Composite parent )
  {
    return new TextCellEditor( parent );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellEditorValidator#isValid(java.lang.Object)
   */
  public String isValid( final Object value )
  {
    try
    {
      final String text = value.toString();
      if( text.length() == 0 )
        return null;
      
      parseData( text );
      return null;
    }
    catch( final Exception e )
    {
      return e.getLocalizedMessage();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getFeatureTypeProperty()
   */
  public FeatureTypeProperty getFeatureTypeProperty()
  {
    return m_ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getLabel(org.deegree.model.feature.Feature)
   */
  public String getLabel( Feature f )
  {
    final Object value = getValue( f );
    return value == null ? "" : value.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getImage(org.deegree.model.feature.Feature)
   */
  public Image getImage( Feature f )
  {
    return null;
  }

}