package org.kalypsodeegree_impl.extension;

import java.net.URL;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.w3c.dom.Node;

/**
 * @author doemming
 */
public class GMLFeatueAssociationTypeHandler implements IMarshallingTypeHandler
{

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getClassName()
   */
  public Class getValueClass( )
  {
    return Feature.class;
  }

  private final QName[] m_typeName = new QName[]{new QName( XMLHelper.GMLSCHEMA_NS, "FeatureAssociationType" )};

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getTypeName()
   */
  public QName[] getTypeName( )
  {
    return m_typeName;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  public void marshall( Object object, Node node, URL context )
  {
    throw new UnsupportedOperationException( "TODO implement it" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  public Object unmarshall( Node node, URL context, IUrlResolver urlResolver )
  {
    throw new UnsupportedOperationException( "TODO implement it" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname( )
  {
    return "Feature link";
  }

  /**
   * @throws CloneNotSupportedException
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone ) throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException( "Clone is not supported" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return true;
  }
}