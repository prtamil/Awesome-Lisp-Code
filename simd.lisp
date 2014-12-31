(eval-when (:compile-toplevel :execute)

(sb-c:defknown %simd-info () (unsigned-byte 32))

(sb-c:define-vop (%simd-info-vop)
  (:translate %simd-info)
  (:results (info :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-byte-32)
  (:policy :fast-safe)
  (:generator
    0
    (sb-c::inst sb-vm::mov sb-vm::eax-tn 1)
    (sb-c::inst sb-vm::cpuid)
    (sb-c::inst sb-vm::and sb-vm::ecx-tn #x10180201)
    (sb-c::inst sb-vm::and sb-vm::edx-tn #x06800000)
    (sb-c::inst sb-vm::or  sb-vm::edx-tn sb-vm::ecx-tn)
    (sb-c::inst sb-vm::mov info          sb-vm::edx-tn)))

) ;;eval-when

(defun simd-info ()
  "(values MMX SSE SSE2 SSE3 SSSE3 SSE4.1 SSE4.2 AVX)"
  (let ((info (%simd-info)))
    (values (logbitp 23 info)
            (logbitp 25 info)
            (logbitp 26 info)
            (logbitp 0  info)
            (logbitp 9  info)
            (logbitp 19 info)
            (logbitp 20 info)
            (logbitp 28 info))))
