//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.0-b26-ea3 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.09.30 at 03:28:54 PM CEST 
//


package com.google.earth.kml;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.google.earth.kml.LodType;
import com.google.earth.kml.ObjectType;


/**
 * <p>Java class for LodType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LodType">
 *   &lt;complexContent>
 *     &lt;extension base="{http://earth.google.com/kml/2.1}ObjectType">
 *       &lt;all>
 *         &lt;element name="minLodPixels" type="{http://www.w3.org/2001/XMLSchema}float" minOccurs="0"/>
 *         &lt;element name="maxLodPixels" type="{http://www.w3.org/2001/XMLSchema}float" minOccurs="0"/>
 *         &lt;element name="minFadeExtent" type="{http://www.w3.org/2001/XMLSchema}float" minOccurs="0"/>
 *         &lt;element name="maxFadeExtent" type="{http://www.w3.org/2001/XMLSchema}float" minOccurs="0"/>
 *       &lt;/all>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "LodType", propOrder = {
    "minLodPixels",
    "maxLodPixels",
    "minFadeExtent",
    "maxFadeExtent"
})
public class LodType
    extends ObjectType
{

    @XmlElement(namespace = "http://earth.google.com/kml/2.1", defaultValue = "0")
    protected Float minLodPixels;
    @XmlElement(namespace = "http://earth.google.com/kml/2.1", defaultValue = "-1")
    protected Float maxLodPixels;
    @XmlElement(namespace = "http://earth.google.com/kml/2.1", defaultValue = "0")
    protected Float minFadeExtent;
    @XmlElement(namespace = "http://earth.google.com/kml/2.1", defaultValue = "0")
    protected Float maxFadeExtent;

    /**
     * Gets the value of the minLodPixels property.
     * 
     * @return
     *     possible object is
     *     {@link Float }
     *     
     */
    public Float getMinLodPixels() {
        return minLodPixels;
    }

    /**
     * Sets the value of the minLodPixels property.
     * 
     * @param value
     *     allowed object is
     *     {@link Float }
     *     
     */
    public void setMinLodPixels(Float value) {
        this.minLodPixels = value;
    }

    /**
     * Gets the value of the maxLodPixels property.
     * 
     * @return
     *     possible object is
     *     {@link Float }
     *     
     */
    public Float getMaxLodPixels() {
        return maxLodPixels;
    }

    /**
     * Sets the value of the maxLodPixels property.
     * 
     * @param value
     *     allowed object is
     *     {@link Float }
     *     
     */
    public void setMaxLodPixels(Float value) {
        this.maxLodPixels = value;
    }

    /**
     * Gets the value of the minFadeExtent property.
     * 
     * @return
     *     possible object is
     *     {@link Float }
     *     
     */
    public Float getMinFadeExtent() {
        return minFadeExtent;
    }

    /**
     * Sets the value of the minFadeExtent property.
     * 
     * @param value
     *     allowed object is
     *     {@link Float }
     *     
     */
    public void setMinFadeExtent(Float value) {
        this.minFadeExtent = value;
    }

    /**
     * Gets the value of the maxFadeExtent property.
     * 
     * @return
     *     possible object is
     *     {@link Float }
     *     
     */
    public Float getMaxFadeExtent() {
        return maxFadeExtent;
    }

    /**
     * Sets the value of the maxFadeExtent property.
     * 
     * @param value
     *     allowed object is
     *     {@link Float }
     *     
     */
    public void setMaxFadeExtent(Float value) {
        this.maxFadeExtent = value;
    }

}
