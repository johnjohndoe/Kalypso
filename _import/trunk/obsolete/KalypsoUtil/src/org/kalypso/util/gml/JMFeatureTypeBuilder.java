package org.kalypso.util.gml;


import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.util.xml.XMLTools;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 */
public class JMFeatureTypeBuilder
{
	private JMFeatureTypeBuilder myParentFeatureProtoType = null;
	private JMSchema mySchema;
	private List myEnumeration = new ArrayList();
	private List myFeatureProtoTypes = new ArrayList();
	private String myName = null;
	private String myTypeName = null;

	private String mySubstitutionGroup = null;
	private boolean myIsNullable = true;

	/**
	 * @param schema 
	 * @param typeName
	 * @param node elementNode
	 * 	<element name="NAModell" type="na:_NAModellType" substitutionGroup="gml:_FeatureCollection">
	
	 * <element name="Knoten" type="na:_KnotenType" substitutionGroup="gml:_Feature"/>
	* <element name="Ausdehnung" type="gml:PolygonPropertyType" substitutionGroup="gml:polygonProperty"/>
	* <element name="minQ" type="double"/>
	* <element name="Landnutzung" type="na:_LandnutzungType"/>
	*
	* but no references like
	*	<element ref="na:Position"/>
	*		
	 * @throws Exception
	 */
	public JMFeatureTypeBuilder(JMSchema schema, Node node) throws Exception
	{

		mySchema = schema;
		myTypeName = null;
		myParentFeatureProtoType = null; //parent;

		Element element = (Element) node;

		if (!element.hasAttribute("name") || !element.hasAttribute("type"))
			throw new Exception(
				"no valid elementnode: " + XMLTools.toString(node));

		JMAttribute nameAttribute =
			new JMAttribute(mySchema, XMLTools.getAttributeNode(node, "name"));
		myName = nameAttribute.getValue();

		JMAttribute typeAttribute =
			new JMAttribute(schema, XMLTools.getAttributeNode(node, "type"));

		//		substitutionGroup...
		if (element.hasAttribute("substitutionGroup"))
		{
			JMAttribute substitutionAttribute =
				new JMAttribute(
					schema,
					XMLTools.getAttributeNode(node, "substitutionGroup"));
			mySubstitutionGroup = substitutionAttribute.getValue();
		}
		setTypeName(typeAttribute);
	}

	public void setTypeName(JMAttribute typeAttribute) throws Exception
	{
		// XMLSchemaType...
		if ("http://www.w3.org/2001/XMLSchema"
			.equals(typeAttribute.getValueNS()))
		{
		
			myTypeName =
				Mapper.mapXMLSchemaType2JavaType(typeAttribute.getValue());
		}
		// GMLSchemaType...
		else if (
			"http://www.opengis.net/gml".equals(typeAttribute.getValueNS()))
		{
			System.out.println("GML Schema Type");
			myTypeName =
				Mapper.mapGMLSchemaType2JavaType(typeAttribute.getValue());
		}
		// local SchemaType
		else if (mySchema.getTargetNS().equals(typeAttribute.getValueNS()))
		{
			System.out.println("local Schema Type");
			Node cNode = getContentNode(mySchema, typeAttribute.getValue());
			if (((Element) cNode).getLocalName().equals("complexType"))
			{
				myTypeName = "org.deegree.model.Feature";
				JMSchemaFactory.map(mySchema, cNode, this);
			}
			else if (((Element) cNode).getLocalName().equals("simpleType"))
			{
				myTypeName = "org.deegree.model.FeatureProperty";
				JMSchemaFactory.map(mySchema, cNode, this);
			}
			else
				throw new Exception(
					"content of element not found in Schema "
						+ typeAttribute.toString());
		}
	}

	private Node getContentNode(JMSchema schema, String type)
	{
		NodeList nl =
			mySchema.getSchema().getElementsByTagNameNS(
				"http://www.w3.org/2001/XMLSchema",
				"complexType");
		nl = XMLTools.reduceByAttribute(nl, "name", type);
		if (nl.getLength() > 0) // complexType ?
			return nl.item(0);
		nl =
			mySchema.getSchema().getElementsByTagNameNS(
				"http://www.w3.org/2001/XMLSchema",
				"simpleType");
		nl = XMLTools.reduceByAttribute(nl, "name", type);
		if (nl.getLength() > 0) // simpleType ?
			return nl.item(0);
		return null;
	}

	public String getName()
	{
		return myName;
	}

	public void add(FeatureTypeProperty featureTypeProperty)
	{
		myFeatureProtoTypes.add(featureTypeProperty);
	}

	public void add(JMFeatureTypeBuilder featureProtoType)
	{
		myFeatureProtoTypes.add(featureProtoType);
		System.out.println("implement...");
	}

	public void addEnumerationObject(Object object)
	{
		myEnumeration.add(object);
	}

	public FeatureTypeProperty toFeatureTypeProperty()
	{
		if (myEnumeration.size() > 0)
			return new EnumerationFeatureTypeProperty(
				myName,
				myTypeName,
				true,
				myEnumeration.toArray());

			return FeatureFactory.createFeatureTypeProperty(
				myName,
				myTypeName,
				true);
	}

	public FeatureType toFeatureType()
	{

		List featureTypeList = new ArrayList();
		List featureTypePropertyList = new ArrayList();

		for (int i = 0; i < myFeatureProtoTypes.size(); i++)
		{
			JMFeatureTypeBuilder builder =
				(JMFeatureTypeBuilder) myFeatureProtoTypes.get(i);

			if (builder.isFeatureType())
				featureTypeList.add(builder.toFeatureType());

			if (builder.isFeaturePropertyType())
				featureTypePropertyList.add(builder.toFeatureTypeProperty());

		}
		FeatureTypeProperty[] ftProperties =
			(FeatureTypeProperty[]) featureTypePropertyList.toArray(
				new FeatureTypeProperty[featureTypePropertyList.size()]);
		FeatureType[] ftChilds =
			(FeatureType[]) featureTypeList.toArray(
				new FeatureType[featureTypeList.size()]);

		return FeatureFactory.createFeatureType(new FeatureType[] { null },
		// parents
		ftChilds, // children
		myName, ftProperties);
	}

	//private FeaturePropertyType();
	private boolean isFeaturePropertyType()
	{
		return myFeatureProtoTypes.size() == 0;

		// TODO
	}

	private boolean isFeatureType()
	{
		return myFeatureProtoTypes.size() > 0;
	}

}
