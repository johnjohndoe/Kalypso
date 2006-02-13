
package org.kalypso.services.sensor.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.sensor.impl.jaxws.GetChildren;

@XmlRootElement(name = "getChildren", namespace = "http://impl.sensor.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "getChildren", namespace = "http://impl.sensor.services.kalypso.org/")
public class GetChildren {

    @XmlElement(name = "arg0", namespace = "")
    private org.kalypso.repository.service.ItemBean arg0;

    /**
     * 
     * @return
     *     returns ItemBean
     */
    public org.kalypso.repository.service.ItemBean getArg0() {
        return this.arg0;
    }

    /**
     * 
     * @param arg0
     *     the value for the arg0 property
     */
    public void setArg0(org.kalypso.repository.service.ItemBean arg0) {
        this.arg0 = arg0;
    }

}
