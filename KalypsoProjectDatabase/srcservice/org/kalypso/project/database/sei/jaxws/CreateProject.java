
package org.kalypso.project.database.sei.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "createProject", namespace = "http://sei.database.project.kalypso.org/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "createProject", namespace = "http://sei.database.project.kalypso.org/")
public class CreateProject {

    @XmlElement(name = "arg0", namespace = "")
    private org.kalypso.project.database.common.interfaces.implementation.KalypsoProjectBeanCreationDelegate arg0;

    /**
     * 
     * @return
     *     returns KalypsoProjectBeanCreationDelegate
     */
    public org.kalypso.project.database.common.interfaces.implementation.KalypsoProjectBeanCreationDelegate getArg0() {
        return this.arg0;
    }

    /**
     * 
     * @param arg0
     *     the value for the arg0 property
     */
    public void setArg0(org.kalypso.project.database.common.interfaces.implementation.KalypsoProjectBeanCreationDelegate arg0) {
        this.arg0 = arg0;
    }

}
