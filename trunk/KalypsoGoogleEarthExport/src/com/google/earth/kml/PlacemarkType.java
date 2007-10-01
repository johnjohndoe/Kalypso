//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.0-b26-ea3 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.09.30 at 03:28:54 PM CEST 
//


package com.google.earth.kml;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlType;

import com.google.earth.kml.FeatureType;
import com.google.earth.kml.GeometryType;
import com.google.earth.kml.LineStringType;
import com.google.earth.kml.LinearRingType;
import com.google.earth.kml.ModelType;
import com.google.earth.kml.MultiGeometryType;
import com.google.earth.kml.PlacemarkType;
import com.google.earth.kml.PointType;
import com.google.earth.kml.PolygonType;


/**
 * <p>Java class for PlacemarkType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PlacemarkType">
 *   &lt;complexContent>
 *     &lt;extension base="{http://earth.google.com/kml/2.1}FeatureType">
 *       &lt;sequence>
 *         &lt;element ref="{http://earth.google.com/kml/2.1}Geometry" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "PlacemarkType", propOrder = {
    "geometry"
})
public class PlacemarkType
    extends FeatureType
{

    @XmlElementRef(name = "Geometry", namespace = "http://earth.google.com/kml/2.1", type = JAXBElement.class)
    protected JAXBElement<? extends GeometryType> geometry;

    /**
     * Gets the value of the geometry property.
     * 
     * @return
     *     possible object is
     *     {@link JAXBElement }{@code <}{@link LinearRingType }{@code >}
     *     {@link JAXBElement }{@code <}{@link PolygonType }{@code >}
     *     {@link JAXBElement }{@code <}{@link MultiGeometryType }{@code >}
     *     {@link JAXBElement }{@code <}{@link GeometryType }{@code >}
     *     {@link JAXBElement }{@code <}{@link LineStringType }{@code >}
     *     {@link JAXBElement }{@code <}{@link ModelType }{@code >}
     *     {@link JAXBElement }{@code <}{@link PointType }{@code >}
     *     
     */
    public JAXBElement<? extends GeometryType> getGeometry() {
        return geometry;
    }

    /**
     * Sets the value of the geometry property.
     * 
     * @param value
     *     allowed object is
     *     {@link JAXBElement }{@code <}{@link LinearRingType }{@code >}
     *     {@link JAXBElement }{@code <}{@link PolygonType }{@code >}
     *     {@link JAXBElement }{@code <}{@link MultiGeometryType }{@code >}
     *     {@link JAXBElement }{@code <}{@link GeometryType }{@code >}
     *     {@link JAXBElement }{@code <}{@link LineStringType }{@code >}
     *     {@link JAXBElement }{@code <}{@link ModelType }{@code >}
     *     {@link JAXBElement }{@code <}{@link PointType }{@code >}
     *     
     */
    public void setGeometry(JAXBElement<? extends GeometryType> value) {
        this.geometry = ((JAXBElement<? extends GeometryType> ) value);
    }

}
