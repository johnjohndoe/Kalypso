package org.kalypsodeegree_impl.extension;

import java.net.URL;

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
  public String getClassName()
  {
    return Feature.class.getName().toString();
  }

  private static String m_nameSpaceURI = XMLHelper.GMLSCHEMA_NS + ":" + "FeatureAssociationType";

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return m_nameSpaceURI;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node, java.net.URL)
   */
  public void marshall( Object object, Node node, URL context )
  {
    throw new UnsupportedOperationException( "TODO implement it" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL, org.kalypso.contribs.java.net.IUrlResolver)
   */
  public Object unmarshall( Node node, URL context, IUrlResolver urlResolver )
  {
    throw new UnsupportedOperationException( "TODO implement it" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname()
  {
    return "Feature link";
  }

}